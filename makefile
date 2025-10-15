# Another World Commander X16
# Makefile for Linux / macOS systems
# --- config --------------------------------------------------------------
AS      := ca65
LD      := ld65

CPU     := w65c02
TARGET  := cx16
INC     := inc
CFG     := cx16-asm.cfg
LIBS    := cx16.lib

BIN     := bin
OBJDIR  := $(BIN)/obj
OUT     := $(BIN)/another.prg

ASFLAGS := --cpu $(CPU) -t $(TARGET) -I $(INC) -g
LDFLAGS := -u __EXEHDR__ -C $(CFG)

SRC := \
  main.s bank.s engine.s input.s irq.s opcodes.s polygon.s resource.s \
  sample.s tasks.s text.s vera.s unpacker.s unpack.s

SRCDIR := src
SRCS   := $(addprefix $(SRCDIR)/,$(SRC))
OBJS   := $(addprefix $(OBJDIR)/,$(SRC:.s=.o))

# --- targets -------------------------------------------------------------
.PHONY: all clean

all: $(OUT)

$(OUT): | $(BIN) $(OBJDIR) $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

# assemble each .s -> .o
$(OBJDIR)/%.o: $(SRCDIR)/%.s | $(OBJDIR)
	$(AS) $(ASFLAGS) -o $@ $<

# ensure dirs exist
$(BIN) $(OBJDIR):
	mkdir -p $@

clean:
	rm -rf $(OBJDIR) $(OUT)
