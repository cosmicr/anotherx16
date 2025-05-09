; ---------------------------------------------------------------
; resource.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global init_resources, load_resource
.global resource_table, next_offset, next_bank

MAX_RESOURCES = 146
RESOURCE_BANK_START = $01

TYPE_SND = 0 ; 8bits raw sound
TYPE_MOD = 1 ; soundfx module
TYPE_BMP = 2 ; 320x200x4 bitmap
TYPE_PAL = 3 ; palette
TYPE_MAC = 4 ; bytecode
TYPE_MAT = 5 ; polygons
TYPE_BNK = 6 ; bank2.mat

.struct resource
    status          .res 1
    rtype           .res 1
    pointer         .res 4
    rank            .res 1
    bank_file       .res 1
    offset          .res 4
    compressed      .res 4
    uncompressed    .res 4
.endstruct

