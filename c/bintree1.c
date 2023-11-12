/*
 *  Binary Tree
 */
#include "bintree1.h"
#include <stdlib.h>

static binnode1 *make_binnode1(binkey1 key, binvalue1 value)
{
	binnode1 *ptr;

	ptr = (binnode1 *)malloc(sizeof(binnode1));
	if (ptr == NULL)
		return NULL;
	ptr->left = ptr->right = NULL;
	ptr->key = key;
	ptr->value = value;

	return ptr;
}

static void free_single_binnode1(binnode1 *ptr)
{
	free(ptr);
}

static void free_binnode1(binnode1 *node)
{
	if (node) {
		free_binnode1(node->left);
		free_binnode1(node->right);
		free_single_binnode1(node);
	}
}

bintree1 *make_bintree1(void)
{
	bintree1 *ptr;

	ptr = (bintree1 *)malloc(sizeof(bintree1));
	if (ptr == NULL)
		return NULL;
	clear_bintree1(ptr);

	return ptr;
}

void free_bintree1(bintree1 *ptr)
{
	if (ptr) {
		free_binnode1(ptr->root);
		free(ptr);
	}
}

void clear_bintree1(bintree1 *ptr)
{
	ptr->root = NULL;
	ptr->size = 0;
}

int compare_bintree1(binkey1 x, binkey1 y, int *ret)
{
	if (x < y)
		*ret = -1;
	else if (y < x)
		*ret = 1;
	else
		*ret = 0;

	return 0;
}


/*
 *  insert
 */
struct insert_bintree1_struct {
	bintree1 *ptr;
	binkey1 key;
	binvalue1 value;
	int replace_mode;
	int result;
};

static int insert_binnode1(struct insert_bintree1_struct *str, binnode1 *node)
{
	int diff;
	binkey1 key, key1;
	binnode1 *next;

	key = str->key;
	for (;;) {
		key1 = node->key;
		if (compare_bintree1(key, key1, &diff))
			return 1;

		/* left */
		if (diff < 0) {
			next = node->left;
			if (next == NULL) {
				next = make_binnode1(key, str->value);
				node->left = next;
				break;
			}
			node = next;
			continue;
		}

		/* right */
		if (0 < diff) {
			next = node->right;
			if (next == NULL) {
				next = make_binnode1(key, str->value);
				node->right = next;
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

static int insert0_bintree1(struct insert_bintree1_struct *str)
{
	bintree1 *ptr;
	binnode1 *root;

	ptr = str->ptr;
	root = ptr->root;
	if (root == NULL) {
		root = make_binnode1(str->key, str->value);
		if (root == NULL)
			return 1;
		ptr->root = root;
		ptr->size = 1;
		str->result = 1;
		return 0;
	}
	else {
		if (insert_binnode1(str, root))
			return 1;
		if (str->result)
			ptr->size++;
		return 0;
	}
}

static int insert_struct_bintree1(bintree1 *ptr,
		binkey1 key, binvalue1 value, int mode, int *ret)
{
	struct insert_bintree1_struct str;

	str.ptr = ptr;
	str.key = key;
	str.value = value;
	str.replace_mode = mode;
	if (insert0_bintree1(&str))
		return 1;
	*ret = str.result;

	return 0;
}

int insert_bintree1(bintree1 *ptr, binkey1 key, binvalue1 value, int *ret)
{
	return insert_struct_bintree1(ptr, key, value, 0, ret);
}

int intern_bintree1(bintree1 *ptr, binkey1 key, binvalue1 value, int *ret)
{
	return insert_struct_bintree1(ptr, key, value, 1, ret);
}


/*
 *  search
 */
static int search_binnode1(bintree1 *ptr, binkey1 key, binnode1 **ret)
{
	int diff;
	binnode1 *node;

	node = ptr->root;
	while (node) {
		if (compare_bintree1(key, node->key, &diff))
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

int search_bintree1(bintree1 *ptr, binkey1 key, binvalue1 *value, int *ret)
{
	binnode1 *node;

	if (search_binnode1(ptr, key, &node))
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

int replace_bintree1(bintree1 *ptr, binkey1 key, binvalue1 value, int *ret)
{
	binnode1 *node;

	if (search_binnode1(ptr, key, &node))
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
static void delete_copy_binnode1(binnode1 *replace, binnode1 *node)
{
	replace->key = node->key;
	replace->value = node->value;
}

static void delete_swap_binnode1(binnode1 *replace, binnode1 *node,
		binnode1 **root, int *ret)
{
	int check;
	binnode1 *next;

	if (node == NULL) {
		*ret = 0;
		return;
	}

	/* left */
	next = node->left;
	delete_swap_binnode1(replace, next, &next, &check);
	if (check) {
		node->left = next;
		*root = node;
		*ret = 1;
		return;
	}

	/* right */
	next = node->right;
	delete_copy_binnode1(replace, node);
	free_single_binnode1(node);
	*root = next;
	*ret = 1;
}

static int delete_equal_binnode1(binnode1 *node, binnode1 **root, int *ret)
{
	int check;
	binnode1 *left, *right, *next;

	/* left, right */
	left = node->left;
	right = node->right;
	if (left && right) {
		delete_swap_binnode1(node, right, &next, &check);
		node->right = next;
		*root = node;
		*ret = 1;
		return 0;
	}

	/* left */
	if (left) {
		free_single_binnode1(node);
		*root = left;
		*ret = 1;
		return 0;
	}

	/* right */
	if (right) {
		free_single_binnode1(node);
		*root = right;
		*ret = 1;
		return 0;
	}

	/* single */
	free_single_binnode1(node);
	*root = NULL;
	*ret = 1;
	return 0;
}

static int delete_binnode1(binnode1 *node, binkey1 key, binnode1 **root, int *ret)
{
	int diff, check;
	binnode1 *next;

	if (node == NULL) {
		*ret = 0;
		return 0;
	}
	if (compare_bintree1(key, node->key, &diff))
		return 1;

	/* left */
	if (diff < 0) {
		next = node->left;
		if (delete_binnode1(next, key, &next, &check))
			return 1;
		if (! check) {
			*ret = 0;
			return 0;
		}
		node->left = next;
		*root = node;
		*ret = 1;
		return 0;
	}

	/* right */
	if (0 < diff) {
		next = node->right;
		if (delete_binnode1(next, key, &next, &check))
			return 1;
		if (! check) {
			*ret = 0;
			return 0;
		}
		node->right = next;
		*root = node;
		*ret = 1;
		return 0;
	}

	/* equal */
	return delete_equal_binnode1(node, root, ret);
}

int delete_bintree1(bintree1 *ptr, binkey1 key, int *ret)
{
	int check;
	binnode1 *node;

	if (delete_binnode1(ptr->root, key, &node, &check))
		return 1;
	if (check) {
		ptr->root = node;
		ptr->size--;
	}
	*ret = check;

	return 0;
}

