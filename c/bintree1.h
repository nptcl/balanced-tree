/*
 *  Binary Tree
 */
#ifndef __BINTREE1_HEADER__
#define __BINTREE1_HEADER__

#include <stddef.h>

typedef int binkey1;
typedef int binvalue1;

struct binnode1_struct {
	struct binnode1_struct *left, *right;
	binkey1 key;
	binvalue1 value;
};

struct bintree1_struct {
	struct binnode1_struct *root;
	size_t size;
};

typedef struct bintree1_struct bintree1;
typedef struct binnode1_struct binnode1;

bintree1 *make_bintree1(void);
void free_bintree1(bintree1 *ptr);
void clear_bintree1(bintree1 *ptr);
int compare_bintree1(binkey1 x, binkey1 y, int *ret);
int insert_bintree1(bintree1 *ptr, binkey1 key, binvalue1 value, int *ret);
int intern_bintree1(bintree1 *ptr, binkey1 key, binvalue1 value, int *ret);
int search_bintree1(bintree1 *ptr, binkey1 key, binvalue1 *value, int *ret);
int replace_bintree1(bintree1 *ptr, binkey1 key, binvalue1 value, int *ret);
int delete_bintree1(bintree1 *ptr, binkey1 key, int *ret);

#endif

