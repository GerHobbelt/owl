void init() {
   int nobjs=0, nwords=0;
   hp = (byte *) &heap; /* builtin heap */
   state = IFALSE;
   heap_metrics(&nwords, &nobjs);
   max_heap_mb = (W == 4) ? 4096 : 65535;
   nwords += nobjs + INITCELLS;
   memstart = genstart = fp = (word *) realloc(NULL, (nwords + MEMPAD)*W);
   if (!memstart) return;
   memend = memstart + nwords - MEMPAD;
   state = (word) load_heap(nobjs);
}

/* bvec → value library call test with preserved state */
word library_call(word val) {
   word program_state = state;
   word res;
   state = IFALSE; 
   if (program_state == IFALSE) {
      exit(1);
   }
   res = vm((word *) program_state, val);
   return res;
}

word tuple_call(uint8_t *ptr, size_t len, size_t max, unsigned int seed) {
   word *arg, res;
   arg = fp;
   fp += 5;
   arg[0] = make_header(5, TTUPLE);
   arg[1] = onum((word)ptr, 0);
   arg[2] = onum(len, 0);
   arg[3] = onum(max, 0);
   arg[4] = onum(seed, 0);
   res = library_call((word) arg);
   return res;
}

void print_list(word ptr) {
   while(ptr != INULL) {
      printf("%d, ", immval(G(ptr, 1)));
      ptr = G(ptr, 2);
   }
   printf("\n");
}

int main(int nargs, char **argv) {
   uint8_t foo[] = {11, 22, 33, 44, 0, 0, 0, 0};
   init();
   print_list(tuple_call((uint8_t *) &foo, 4, 8, 42));
   print_list(tuple_call((uint8_t *) &foo, 3, 9, 43));
   print_list(tuple_call((uint8_t *) &foo, 1, 9, 44));
   return 0;
}


