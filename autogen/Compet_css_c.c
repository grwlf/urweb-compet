/* Thanks, http://stupefydeveloper.blogspot.ru/2008/08/cc-embed-binary-data-into-elf.html */
#include <urweb.h>
#include <stdio.h>
#define BLOBSZ 30
static char blob[BLOBSZ];
uw_Basis_blob uw_Compet_css_c_binary (uw_context ctx, uw_unit unit)
{
  uw_Basis_blob uwblob;
  uwblob.data = &blob[0];
  uwblob.size = BLOBSZ;
  return uwblob;
}

uw_Basis_string uw_Compet_css_c_text (uw_context ctx, uw_unit unit) {
  char* data = &blob[0];
  size_t size = sizeof(blob);
  char * c = uw_malloc(ctx, size+1);
  char * write = c;
  int i;
  for (i = 0; i < size; i++) {
    *write =  data[i];
    if (*write == '\0')
      *write = '\n';
    *write++;
  }
  *write=0;
  return c;
  }


static char blob[BLOBSZ] = {
0x62 ,0x6F ,0x64 ,0x79 ,0x20 ,0x7B ,0x0A ,0x20 ,0x20 ,0x70 ,0x61 ,0x64 ,0x64 ,0x69 ,0x6E ,0x67 ,0x2D ,0x74 ,0x6F ,0x70 ,0x3A ,0x20 ,0x35 ,0x30 ,0x70 ,0x78 ,0x3B ,0x0A ,0x7D ,0x0A ,};

