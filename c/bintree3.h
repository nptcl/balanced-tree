/*
 *  Binary Tree
 */
#ifndef __BINTREE3_HEADER__
#define __BINTREE3_HEADER__

#include <stddef.h>

typedef int binkey3;
typedef int binvalue3;

struct binnode3_struct {
	struct binnode3_struct *parent, *left, *right;
	binkey3 key;
	binvalue3 value;
};

struct bintree3_struct {
	struct binnode3_struct *root;
	size_t size;
};

typedef struct bintree3_struct bintree3;
typedef struct binnode3_struct binnode3;

bintree3 *make_bintree3(void);
void free_bintree3(bintree3 *ptr);
void clear_bintree3(bintree3 *ptr);
int compare_bintree3(binkey3 x, binkey3 y, int *ret);
binnode3 *min_bintree3(bintree3 *ptr);
binnode3 *max_bintree3(bintree3 *ptr);
binnode3 *next_binnode3(binnode3 *x);
binnode3 *prev_binnode3(binnode3 *x);
int insert_bintree3(bintree3 *ptr, binkey3 key, binvalue3 value, int *ret);
int intern_bintree3(bintree3 *ptr, binkey3 key, binvalue3 value, int *ret);
int search_bintree3(bintree3 *ptr, binkey3 key, binvalue3 *value, int *ret);
int replace_bintree3(bintree3 *ptr, binkey3 key, binvalue3 value, int *ret);
int delete_bintree3(bintree3 *ptr, binkey3 key, int *ret);

#endif

