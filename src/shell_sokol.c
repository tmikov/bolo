// bolo - public domain Tzvetan Mikov 2021
//
// The sokol front end: window, rendering, audio and keyboard. All game logic
// lives in bolo.c and is driven through bolo.h.

#include "sokol_app.h"
#include "sokol_audio.h"
#include "sokol_gfx.h"
#include "sokol_glue.h"
#include "sokol_time.h"

#include "blit.h"

#include "bolo.h"
#include "ega_render.h"

#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

/// The EGA bitplanes converted to RGB here.
static RGBA8 g_rgb_screen[EGA_WIDTH_POT * EGA_HEIGHT_POT];

#define SOUND_QUEUE_CAPACITY 8192

typedef struct {
  /// Index of next element to read. Accessed only by the reader thread.
  int head;
  /// Index of next element write. Accessed only by the writer thread.
  int tail;
  /// Number of elements in queue. Shared by the reader and writer thread.
  atomic_int count;
  /// The actual data.
  float samples[SOUND_QUEUE_CAPACITY];
} sound_queue_t;

static struct {
  sg_pass_action pass_action;
  sg_pipeline pip;
  sg_bindings bind;

  double lastTimerTickS;
  sound_queue_t fx;
} state;

static void sound_queue_init(sound_queue_t *q) {
  q->head = 0;
  q->tail = 0;
  atomic_init(&q->count, 0);
}

static int sound_queue_expect(sound_queue_t *q) {
  return SOUND_QUEUE_CAPACITY - atomic_load_explicit(&q->count, memory_order_acquire);
}

static int sound_queue_count(sound_queue_t *q) {
  return atomic_load_explicit(&q->count, memory_order_acquire);
}

typedef struct {
  float *part1;
  int size1;
  float *part2;
  int size2;
} queue_parts_t;

static queue_parts_t sound_queue_writeparts(sound_queue_t *q, int len) {
  int expect = sound_queue_expect(q);
  if (len > expect)
    len = expect;

  if (len <= SOUND_QUEUE_CAPACITY - q->tail) {
    return (queue_parts_t){.part1 = q->samples + q->tail, .size1 = len};
  } else {
    int toCopy = SOUND_QUEUE_CAPACITY - q->tail;
    return (queue_parts_t){
        .part1 = q->samples + q->tail, .size1 = toCopy, .part2 = q->samples, .size2 = len - toCopy};
  }
}

static queue_parts_t sound_queue_readparts(sound_queue_t *q, int len) {
  int count = sound_queue_count(q);
  if (len > count)
    len = count;

  if (len <= SOUND_QUEUE_CAPACITY - q->head) {
    return (queue_parts_t){.part1 = q->samples + q->head, .size1 = len};
  } else {
    int toCopy = SOUND_QUEUE_CAPACITY - q->head;
    return (queue_parts_t){
        .part1 = q->samples + q->head, .size1 = toCopy, .part2 = q->samples, .size2 = len - toCopy};
  }
}

static void sound_queue_adv_tail(sound_queue_t *q, int len) {
  q->tail = (q->tail + len) & (SOUND_QUEUE_CAPACITY - 1);
  atomic_fetch_add_explicit(&q->count, len, memory_order_release);
}

static void sound_queue_adv_head(sound_queue_t *q, int len) {
  q->head = (q->head + len) & (SOUND_QUEUE_CAPACITY - 1);
  atomic_fetch_add_explicit(&q->count, -len, memory_order_release);
}

static int sound_queue_push(sound_queue_t *q, const float *data, int len) {
  queue_parts_t parts = sound_queue_writeparts(q, len);
  memcpy(parts.part1, data, parts.size1 * sizeof(float));
  if (parts.size2)
    memcpy(parts.part2, data + parts.size1, parts.size2 * sizeof(float));
  sound_queue_adv_tail(q, parts.size1 + parts.size2);
  return parts.size1 + parts.size2;
}

static int sound_queue_pop(sound_queue_t *q, float *data, int len) {
  queue_parts_t parts = sound_queue_readparts(q, len);
  memcpy(data, parts.part1, sizeof(float) * parts.size1);
  if (parts.size2)
    memcpy(data + parts.size1, parts.part2, sizeof(float) * parts.size2);
  sound_queue_adv_head(q, parts.size1 + parts.size2);
  return parts.size1 + parts.size2;
}

static void bolo_sound_cb(float *buffer, int num_frames, int num_channels) {
  int popped;
  if (num_channels == 1) {
    popped = sound_queue_pop(&state.fx, buffer, num_frames);
  } else if (num_channels == 2) {
    queue_parts_t parts = sound_queue_readparts(&state.fx, num_frames);
    int i;
    for (i = 0; i < parts.size1; ++i) {
      buffer[0] = buffer[1] = parts.part1[i];
      buffer += 2;
    }
    for (i = 0; i < parts.size2; ++i) {
      buffer[0] = buffer[1] = parts.part2[i];
      buffer += 2;
    }
    popped = parts.size1 + parts.size2;
    sound_queue_adv_head(&state.fx, popped);
  } else {
    popped = 0;
  }

  if (popped < num_frames) {
    // Fill the rest of the frame with zeroes.
    memset(buffer + popped * num_channels, 0, sizeof(float) * (num_frames - popped) * num_channels);
  }
}

static void bolo_update_screen() {
  const uint8_t *planes[EGA_PLANES] = {
      bolo_plane(0),
      bolo_plane(1),
      bolo_plane(2),
      bolo_plane(3),
  };
  ega_screen_to_rgba(planes, bolo_palette(), g_rgb_screen, EGA_WIDTH_POT);

  sg_update_image(
      state.bind.fs_images[SLOT_tex],
      &(sg_image_data){.subimage[0][0] = {.ptr = g_rgb_screen, .size = sizeof(g_rgb_screen)}});
}

/*
  This is how BOLO generates sound. CH is the inner loop delay, CL is how many
  times to flip the speaker. 8088 instruction cycles are written after every
  instruction.
                  mov     cl,30

  locloop_146:
                  mov     ch,32          [ 4]
                  in      al,61h         [14]     ; port 61h, 8255 port B, read
                  xor     al,data_180    [19]     ; (2913:2F66=2)
                  out     61h,al         [14]     ; port 61h, 8255 B - spkr, etc
  loc_147:
                  dec     ch             [ 3]
                  jnz     loc_147        [16 or 4]
                  loop    locloop_146    [17 or 5]

  Sound flip: 4+14+19+14 = 51
  Delay loop: (CH-1)*19 + 7 = 19*CH - 19 + 7 = 19*CH - 12
  Loop period: 51 + 19*CH - 12 + 17 = 19*CH + 56 cycles
  Sound freq: 4.77MHz / (loop period * 2)
  One cycle at 4.77MHz is 210ns (210e-9).
  Loop period:

  In this case: 19*32 + 56 = 664;  4.77e6 / 664 = 3592Hz for 4.1 ms
 */

/// Synthesize the original's bit-banged PC speaker square wave and queue it.
static void shell_play_sound(int ch_delay, int cl_length) {
  static const int kCPUFreq = 4770000;
  int loopPeriodCyc = (19 * ch_delay + 56) * 2;
  int loopRate = kCPUFreq / loopPeriodCyc;

  enum { kBufSize = 2048 };
  static float buffer[kBufSize];

  const int sampRate = saudio_sample_rate();
  int outLen = sampRate * cl_length / loopRate;
  // In the unlikely even that the buffer is not sufficient, truncate.
  if (outLen > kBufSize)
    outLen = kBufSize;

  float *p = buffer;
  float input = -0.1f;
  int acc = 0;
  int cnt = outLen;
  while (cnt--) {
    *p++ = input;
    acc += loopRate;
    while (acc >= sampRate) {
      acc -= sampRate;
      input = -input;
    }
  }

  sound_queue_push(&state.fx, buffer, outLen);
}

static void bolo_init(void) {
  bolo_reset();
  bolo_sound_sink = shell_play_sound;

  saudio_setup(&(saudio_desc){
      //.sample_rate = 44100,
      //.buffer_frames = 2048,
      .stream_cb = bolo_sound_cb,
      .num_channels = 1,
  });

  stm_setup();
  state.lastTimerTickS = stm_sec(stm_now());

  sg_setup(&(sg_desc){.context = sapp_sgcontext()});

  state.pass_action = (sg_pass_action){.colors[0] = {.action = SG_ACTION_CLEAR}};

  state.bind.fs_images[SLOT_tex] = sg_make_image(&(sg_image_desc){
      .width = EGA_WIDTH_POT,
      .height = EGA_HEIGHT_POT,
      .usage = SG_USAGE_STREAM,
      .min_filter = SG_FILTER_LINEAR,
      .mag_filter = SG_FILTER_LINEAR,
      .label = "ega_image",
  });

  /*
   * Triangle strip:
   *    2  |  0
   * ------+------
   *    3  |  1
   */
  static const float U = (float)EGA_WIDTH / EGA_WIDTH_POT;
  static const float V = (float)EGA_HEIGHT / EGA_HEIGHT_POT;
  static const float vertices[][4] = {
      {1, 1, U, 0},
      {1, -1, U, V},
      {-1, 1, 0, 0},
      {-1, -1, 0, V},
  };
  state.bind.vertex_buffers[0] = sg_make_buffer(&(sg_buffer_desc){
      .data = SG_RANGE(vertices),
      .label = "rect vertices",
  });

  sg_shader blit = sg_make_shader(blit_shader_desc(sg_query_backend()));

  state.pip = sg_make_pipeline(&(sg_pipeline_desc){
      .shader = blit,
      .layout =
          {.attrs =
               {
                   [ATTR_vs_pos].format = SG_VERTEXFORMAT_FLOAT2,
                   [ATTR_vs_texcoord0].format = SG_VERTEXFORMAT_FLOAT2,
               }},
      .primitive_type = SG_PRIMITIVETYPE_TRIANGLE_STRIP,
      .label = "rect pipeline",
  });
}

static void bolo_frame(void) {
  double newTime = stm_sec(stm_now());

  while (newTime - state.lastTimerTickS >= TIMER_PERIOD_US * 1e-6) {
    state.lastTimerTickS += TIMER_PERIOD_US * 1e-6;
    bolo_timer_tick();
  }

  bolo_step();
  bolo_update_screen();

  sg_begin_default_pass(&state.pass_action, sapp_width(), sapp_height());

  {
    // We always preserve the 320x200 aspect ratio. We don't care about the
    // 1.2x1 pixel aspect ratio of the original CRT monitors.
    int w = sapp_width();
    int h = sapp_height();
    int desiredW, desiredH;

    if (w * EGA_HEIGHT / h >= EGA_WIDTH) {
      desiredH = h;
      desiredW = h * EGA_WIDTH / EGA_HEIGHT;
    } else {
      desiredW = w;
      desiredH = w * EGA_HEIGHT / EGA_WIDTH;
    }
    sg_apply_viewport((w - desiredW) / 2, (h - desiredH) / 2, desiredW, desiredH, true);
  }

  sg_apply_pipeline(state.pip);
  sg_apply_bindings(&state.bind);
  sg_draw(0, 4, 1);
  sg_end_pass();
  sg_commit();
}

static void bolo_cleanup(void) {
  sg_shutdown();
  saudio_shutdown();
}

static uint8_t to_scan_code(sapp_keycode keycode) {
  switch (keycode) {
  case SAPP_KEYCODE_ESCAPE:
    return SC_ESC;
  case SAPP_KEYCODE_ENTER:
    return SC_ENTER;
  case SAPP_KEYCODE_SPACE:
    return SC_SPACE;
  case SAPP_KEYCODE_0:
    return SC_0;
  case SAPP_KEYCODE_LEFT:
    return SC_LEFT;
  case SAPP_KEYCODE_RIGHT:
    return SC_RIGHT;
  case SAPP_KEYCODE_UP:
    return SC_UP;
  case SAPP_KEYCODE_DOWN:
    return SC_DOWN;

  case SAPP_KEYCODE_W:
    return SC_W;
  case SAPP_KEYCODE_A:
    return SC_A;
  case SAPP_KEYCODE_S:
    return SC_S;
  case SAPP_KEYCODE_D:
    return SC_D;
  case SAPP_KEYCODE_X:
    return SC_X;
  case SAPP_KEYCODE_B:
    return SC_B;
  default:
    break;
  }

  if (keycode >= SAPP_KEYCODE_1 && keycode <= SAPP_KEYCODE_9) {
    return SC_1 + keycode - SAPP_KEYCODE_1;
  }
  if (keycode >= SAPP_KEYCODE_F1 && keycode <= SAPP_KEYCODE_F10) {
    return SC_F1 + keycode - SAPP_KEYCODE_F1;
  }

  return 0;
}

static void bolo_event(const sapp_event *ev) {
  if (ev->type == SAPP_EVENTTYPE_KEY_DOWN) {
    uint8_t sc = to_scan_code(ev->key_code);
    if (sc)
      bolo_key(sc);
  }
}

sapp_desc sokol_main(int argc, char *argv[]) {
  (void)argc;
  (void)argv;
  return (sapp_desc){
      .init_cb = bolo_init,
      .frame_cb = bolo_frame,
      .cleanup_cb = bolo_cleanup,
      .event_cb = bolo_event,
      .width = EGA_WIDTH * 2,
      .height = EGA_HEIGHT * 2,
      .window_title = "Bolo",
      .icon.sokol_default = true,
  };
}
