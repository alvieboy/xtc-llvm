Description := XTC port

Configs := xtc
Arch := xtc

CC := clang

CFLAGS := -Wall -D_BIG_ENDIAN -Werror -O3 -target xtc-elf

FUNCTIONS := moddi3 floatundixf udivdi3 muldi3 udivsi3 adddf3 muldf3 divdf3 \
subdf3 fixsfsi fixdfdi comparedf2 comparesf2 floatsidf fixdfsi floatunsidf \
modsi3 divsi3 umoddi3 udivmoddi4 umodsi3 fixunsdfsi \
divdi3 udivdi3 \
ashldi3 lshrdi3 
