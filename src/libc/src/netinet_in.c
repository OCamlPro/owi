#include <netinet/in.h>

uint32_t htonl(uint32_t hostlong) {
  return (hostlong >> 24) | ((hostlong & 0xff0000) >> 8) |
         ((hostlong & 0xff00) << 8) | (hostlong << 24);
}

uint16_t htons(uint16_t hostshort) {
  return ((hostshort >> 8) & 0xff) | (hostshort << 8);
}

uint32_t ntohl(uint32_t hostlong) __attribute__((weak, alias("htonl")));
uint16_t ntohs(uint16_t hostshort) __attribute__((weak, alias("htons")));
