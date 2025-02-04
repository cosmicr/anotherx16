; ---------------------------------------------------------------
; resource.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global init_resources, load_resource
.global resource_table, next_offset, next_bank

MAX_RESOURCES = 146
RESOURCE_BANK_START = $01

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

