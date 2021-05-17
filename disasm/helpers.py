def dec40(ofs):
    print "VID_OFFSET(%d, %d)" % (ofs % 40 * 8, ofs/40)
def dec64(ofs):
    print "%d*64 + %d" % (ofs / 64, ofs % 64)
