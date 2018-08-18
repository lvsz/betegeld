# =============================================================================
# Project makefile
# =============================================================================

# Settings - Edit the names of the object and executable files.
objs = rand.obj mouse.obj keyb.obj main.obj
dest = betegeld.exe

# =============================================================================
# Do NOT edit this section, unless you know what you are doing.
# =============================================================================
.asm.obj
  # use TASM with debug info (stripped anyway by wlink if not used)
  tasm /ml /m2 /w2 /z /zi /p /jIDEAL $<

$(dest): $(objs)
  wlink option quiet system pmodew name $@ file {$(objs)}
  echo Destination:       $(dest)

clean: .symbolic
  del *.obj
  del $(dest)

.silent
debug: $(objs) .symbolic
  wlink debug watcom all option quiet system pmodew name $(dest) file {$(objs)}
  wd /tr=rsi /li=43 $(dest)
