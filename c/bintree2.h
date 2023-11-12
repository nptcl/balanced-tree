/*
 *  Binary Tree
 */
#ifndef __BINTREE2_HEADER__
#define __BINTREE2_HEADER__

#include <stddef.h>

typedef int binkey2;
typedef int binvalue2;

struct binnode2_struct {
	struct binnode2_struct *parent, *left, *right;
	binkey2 key;
	binvalue2 value;
};

struct bintree2_struct {
	struct binnode2_struct *root;
	size_t size;
};

typedef struct bintree2_struct bintree2;
typedef struct binnode2_struct binnode2;

bintree2 *make_bintree2(void);
void free_bintree2(bintree2 *ptr);
void clear_bintree2(bintree2 *ptr);
int compare_bintree2(binkey2 x, binkey2 y, int *ret);
binnode2 *min_bintree2(bintree2 *ptr);
binnode2 *max_bintree2(bintree2 *ptr);
binnode2 *next_binnode2(binnode2 *x);
binnode2 *prev_binnode2(binnode2 *x);
int insert_bintree2(bintree2 *ptr, binkey2 key, binvalue2 value, int *ret);
int intern_bintree2(bintree2 *ptr, binkey2 key, binvalue2 value, int *ret);
int search_bintree2(bintree2 *ptr, binkey2 key, binvalue2 *value, int *ret);
int replace_bintree2(bintree2 *ptr, binkey2 key, binvalue2 value, int *ret);
int delete_bintree2(bintree2 *ptr, binkey2 key, int *ret);

#endif

