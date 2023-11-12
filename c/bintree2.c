/*
 *  Binary Tree  (parent link)
 */
#include "bintree2.h"
#include <stdlib.h>

static binnode2 *make_binnode2(binkey2 key, binvalue2 value)
{
	binnode2 *ptr;

	ptr = (binnode2 *)malloc(sizeof(binnode2));
	if (ptr == NULL)
		return NULL;
	ptr->parent = ptr->left = ptr->right = NULL;
	ptr->key = key;
	ptr->value = value;

	return ptr;
}

static void free_single_binnode2(binnode2 *ptr)
{
	free(ptr);
}

static void free_binnode2(binnode2 *node)
{
	if (node) {
		free_binnode2(node->left);
		free_binnode2(node->right);
		free_single_binnode2(node);
	}
}

bintree2 *make_bintree2(void)
{
	bintree2 *ptr;

	ptr = (bintree2 *)malloc(sizeof(bintree2));
	if (ptr == NULL)
		return NULL;
	clear_bintree2(ptr);

	return ptr;
}

void free_bintree2(bintree2 *ptr)
{
	if (ptr) {
		free_binnode2(ptr->root);
		free(ptr);
	}
}

void clear_bintree2(bintree2 *ptr)
{
	ptr->root = NULL;
	ptr->size = 0;
}

int compare_bintree2(binkey2 x, binkey2 y, int *ret)
{
	if (x < y)
		*ret = -1;
	else if (y < x)
		*ret = 1;
	else
		*ret = 0;

	return 0;
}


/* parent */
binnode2 *min_bintree2(bintree2 *ptr)
{
	binnode2 *node, *next;

	node = ptr->root;
	if (node == NULL)
		return NULL;
	for (;;) {
		next = node->left;
		if (next == NULL)
			break;
		node = next;
	}

	return node;
}

binnode2 *max_bintree2(bintree2 *ptr)
{
	binnode2 *node, *next;

	node = ptr->root;
	if (node == NULL)
		return NULL;
	for (;;) {
		next = node->right;
		if (next == NULL)
			break;
		node = next;
	}

	return node;
}

binnode2 *next_binnode2(binnode2 *x)
{
	binnode2 *y, *z;

	/* loop */
	y = x->right;
	if (y) {
		for (;;) {
			x = y->left;
			if (x == NULL)
				break;
			y = x;
		}
		return y;
	}

	/* parent */
	y = x->parent;
	while (y) {
		z = y->right;
		if (x != z)
			break;
		x = y;
		y = y->parent;
	}
	return y;
}

binnode2 *prev_binnode2(binnode2 *x)
{
	binnode2 *y, *z;

	/* loop */
	y = x->left;
	if (y) {
		for (;;) {
			x = y->right;
			if (x == NULL)
				break;
			y = x;
		}
		return y;
	}

	/* parent */
	y = x->parent;
	while (y) {
		z = y->left;
		if (x != z)
			break;
		x = y;
		y = y->parent;
	}
	return y;
}


/*
 *  insert
 */
struct insert_bintree2_struct {
	bintree2 *ptr;
	binkey2 key;
	binvalue2 value;
	int replace_mode;
	int result;
};

static int insert_binnode2(struct insert_bintree2_struct *str, binnode2 *node)
{
	int diff;
	binkey2 key, key1;
	binnode2 *next;

	key = str->key;
	for (;;) {
		key1 = node->key;
		if (compare_bintree2(key, key1, &diff))
			return 1;

		/* left */
		if (diff < 0) {
			next = node->left;
			if (next == NULL) {
				next = make_binnode2(key, str->value);
				node->left = next;
				next->parent = node;
				break;
			}
			node = next;
			continue;
		}

		/* right */
		if (0 < diff) {
			next = node->right;
			if (next == NULL) {
				next = make_binnode2(key, str->value);
				node->right = next;
				next->parent = node;
				break;
			}
			node = next;
			continue;
		}

		/* equal */
		if (str->replace_mode)
			node->value = str->value;
		str->result = 0;
		return 0;
	}

	/* insert */
	str->result = 1;
	return 0;
}

static int insert0_bintree2(struct insert_bintree2_struct *str)
{
	bintree2 *ptr;
	binnode2 *root;

	ptr = str->ptr;
	root = ptr->root;
	if (root == NULL) {
		root = make_binnode2(str->key, str->value);
		if (root == NULL)
			return 1;
		ptr->root = root;
		ptr->size = 1;
		str->result = 1;
		return 0;
	}
	else {
		if (insert_binnode2(str, root))
			return 1;
		if (str->result)
			ptr->size++;
		return 0;
	}
}

static int insert_struct_bintree2(bintree2 *ptr,
		binkey2 key, binvalue2 value, int mode, int *ret)
{
	struct insert_bintree2_struct str;

	str.ptr = ptr;
	str.key = key;
	str.value = value;
	str.replace_mode = mode;
	if (insert0_bintree2(&str))
		return 1;
	*ret = str.result;

	return 0;
}

int insert_bintree2(bintree2 *ptr, binkey2 key, binvalue2 value, int *ret)
{
	return insert_struct_bintree2(ptr, key, value, 0, ret);
}

int intern_bintree2(bintree2 *ptr, binkey2 key, binvalue2 value, int *ret)
{
	return insert_struct_bintree2(ptr, key, value, 1, ret);
}


/*
 *  search
 */
static int search_binnode2(bintree2 *ptr, binkey2 key, binnode2 **ret)
{
	int diff;
	binnode2 *node;

	node = ptr->root;
	while (node) {
		if (compare_bintree2(key, node->key, &diff))
			return 1;
		if (diff < 0) {
			node = node->left;
		}
		else if (0 < diff) {
			node = node->right;
		}
		else {
			*ret = node;
			return 0;
		}
	}

	/* not found */
	*ret = NULL;
	return 0;
}

int search_bintree2(bintree2 *ptr, binkey2 key, binvalue2 *value, int *ret)
{
	binnode2 *node;

	if (search_binnode2(ptr, key, &node))
		return 1;
	if (node) {
		*value = node->value;
		*ret = 1;
		return 0;
	}
	else {
		*ret = 0;
		return 0;
	}
}

int replace_bintree2(bintree2 *ptr, binkey2 key, binvalue2 value, int *ret)
{
	binnode2 *node;

	if (search_binnode2(ptr, key, &node))
		return 1;
	if (node == NULL) {
		*ret = 0;
		return 0;
	}
	else {
		node->value = value;
		*ret = 1;
		return 0;
	}
}


/*
 *  delete
 */
static void delete_copy_binnode2(binnode2 *replace, binnode2 *node)
{
	replace->key = node->key;
	replace->value = node->value;
}

static void delete_swap_binnode2(binnode2 *replace, binnode2 *node,
		binnode2 **root, int *ret)
{
	int check;
	binnode2 *next;

	if (node == NULL) {
		*ret = 0;
		return;
	}

	/* left */
	next = node->left;
	delete_swap_binnode2(replace, next, &next, &check);
	if (check) {
		node->left = next;
		if (next)
			next->parent = node;
		*root = node;
		*ret = 1;
		return;
	}

	/* right */
	next = node->right;
	delete_copy_binnode2(replace, node);
	free_single_binnode2(node);
	*root = next;
	*ret = 1;
}

static int delete_equal_binnode2(binnode2 *node, binnode2 **root, int *ret)
{
	int check;
	binnode2 *left, *right, *next;

	/* left, right */
	left = node->left;
	right = node->right;
	if (left && right) {
		delete_swap_binnode2(node, right, &next, &check);
		node->right = next;
		if (next)
			next->parent = node;
		*root = node;
		*ret = 1;
		return 0;
	}

	/* left */
	if (left) {
		free_single_binnode2(node);
		*root = left;
		*ret = 1;
		return 0;
	}

	/* right */
	if (right) {
		free_single_binnode2(node);
		*root = right;
		*ret = 1;
		return 0;
	}

	/* single */
	free_single_binnode2(node);
	*root = NULL;
	*ret = 1;
	return 0;
}

static int delete_binnode2(binnode2 *node, binkey2 key, binnode2 **root, int *ret)
{
	int diff, check;
	binnode2 *next;

	if (node == NULL) {
		*ret = 0;
		return 0;
	}
	if (compare_bintree2(key, node->key, &diff))
		return 1;

	/* left */
	if (diff < 0) {
		next = node->left;
		if (delete_binnode2(next, key, &next, &check))
			return 1;
		if (! check) {
			*ret = 0;
			return 0;
		}
		node->left = next;
		if (next)
			next->parent = node;
		*root = node;
		*ret = 1;
		return 0;
	}

	/* right */
	if (0 < diff) {
		next = node->right;
		if (delete_binnode2(next, key, &next, &check))
			return 1;
		if (! check) {
			*ret = 0;
			return 0;
		}
		node->right = next;
		if (next)
			next->parent = node;
		*root = node;
		*ret = 1;
		return 0;
	}

	/* equal */
	return delete_equal_binnode2(node, root, ret);
}

int delete_bintree2(bintree2 *ptr, binkey2 key, int *ret)
{
	int check;
	binnode2 *node;

	if (delete_binnode2(ptr->root, key, &node, &check))
		return 1;
	if (check) {
		ptr->root = node;
		if (node)
			node->parent = NULL;
		ptr->size--;
	}
	*ret = check;

	return 0;
}

