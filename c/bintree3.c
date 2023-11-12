/*
 *  Binary Tree  (fixup)
 */
#include "bintree3.h"
#include <stdlib.h>

static binnode3 *make_binnode3(binkey3 key, binvalue3 value)
{
	binnode3 *ptr;

	ptr = (binnode3 *)malloc(sizeof(binnode3));
	if (ptr == NULL)
		return NULL;
	ptr->parent = ptr->left = ptr->right = NULL;
	ptr->key = key;
	ptr->value = value;

	return ptr;
}

static void free_single_binnode3(binnode3 *ptr)
{
	free(ptr);
}

static void free_binnode3(binnode3 *node)
{
	if (node) {
		free_binnode3(node->left);
		free_binnode3(node->right);
		free_single_binnode3(node);
	}
}

bintree3 *make_bintree3(void)
{
	bintree3 *ptr;

	ptr = (bintree3 *)malloc(sizeof(bintree3));
	if (ptr == NULL)
		return NULL;
	clear_bintree3(ptr);

	return ptr;
}

void free_bintree3(bintree3 *ptr)
{
	if (ptr) {
		free_binnode3(ptr->root);
		free(ptr);
	}
}

void clear_bintree3(bintree3 *ptr)
{
	ptr->root = NULL;
	ptr->size = 0;
}

int compare_bintree3(binkey3 x, binkey3 y, int *ret)
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
binnode3 *min_bintree3(bintree3 *ptr)
{
	binnode3 *node, *next;

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

binnode3 *max_bintree3(bintree3 *ptr)
{
	binnode3 *node, *next;

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

binnode3 *next_binnode3(binnode3 *x)
{
	binnode3 *y, *z;

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

binnode3 *prev_binnode3(binnode3 *x)
{
	binnode3 *y, *z;

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
 *  insert-fixup
 */
static void insert_fixup_bintree3(bintree3 *ptr, binnode3 *node)
{
	/* do nothing */
}


/*
 *  insert
 */
struct insert_bintree3_struct {
	bintree3 *ptr;
	binnode3 *node;
	binkey3 key;
	binvalue3 value;
	int replace_mode;
	int result;
};

static int insert_binnode3(struct insert_bintree3_struct *str, binnode3 *node)
{
	int diff;
	binkey3 key, key1;
	binnode3 *next;

	key = str->key;
	for (;;) {
		key1 = node->key;
		if (compare_bintree3(key, key1, &diff))
			return 1;

		/* left */
		if (diff < 0) {
			next = node->left;
			if (next == NULL) {
				next = make_binnode3(key, str->value);
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
				next = make_binnode3(key, str->value);
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
	str->node = next;
	str->result = 1;
	return 0;
}

static int insert0_bintree3(struct insert_bintree3_struct *str)
{
	bintree3 *ptr;
	binnode3 *root;

	ptr = str->ptr;
	root = ptr->root;
	if (root == NULL) {
		root = make_binnode3(str->key, str->value);
		if (root == NULL)
			return 1;
		ptr->root = root;
		ptr->size = 1;
		str->node = root;
		str->result = 1;
		return 0;
	}
	else {
		if (insert_binnode3(str, root))
			return 1;
		if (str->result)
			ptr->size++;
		return 0;
	}
}

static int insert_struct_bintree3(bintree3 *ptr,
		binkey3 key, binvalue3 value, int mode, int *ret)
{
	struct insert_bintree3_struct str;

	str.ptr = ptr;
	str.node = NULL;
	str.key = key;
	str.value = value;
	str.replace_mode = mode;
	if (insert0_bintree3(&str))
		return 1;
	if (str.result)
		insert_fixup_bintree3(ptr, str.node);
	*ret = str.result;

	return 0;
}

int insert_bintree3(bintree3 *ptr, binkey3 key, binvalue3 value, int *ret)
{
	return insert_struct_bintree3(ptr, key, value, 0, ret);
}

int intern_bintree3(bintree3 *ptr, binkey3 key, binvalue3 value, int *ret)
{
	return insert_struct_bintree3(ptr, key, value, 1, ret);
}


/*
 *  search
 */
static int search_binnode3(bintree3 *ptr, binkey3 key, binnode3 **ret)
{
	int diff;
	binnode3 *node;

	node = ptr->root;
	while (node) {
		if (compare_bintree3(key, node->key, &diff))
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

int search_bintree3(bintree3 *ptr, binkey3 key, binvalue3 *value, int *ret)
{
	binnode3 *node;

	if (search_binnode3(ptr, key, &node))
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

int replace_bintree3(bintree3 *ptr, binkey3 key, binvalue3 value, int *ret)
{
	binnode3 *node;

	if (search_binnode3(ptr, key, &node))
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
 *  delete-fixup
 */
static void delete_fixup_bintree3(bintree3 *ptr,
		binnode3 *parent, binnode3 *child, int direct)
{
	/* do nothing */
}


/*
 *  delete
 */
static void delete_copy_binnode3(binnode3 *replace, binnode3 *node)
{
	replace->key = node->key;
	replace->value = node->value;
}

static void delete_single_binnode3(bintree3 *ptr, binnode3 *delete, binnode3 *child)
{
	int direct;
	binnode3 *parent;

	parent = delete->parent;
	if (parent == NULL) {
		ptr->root = child;
		direct = 0;
	}
	else if (parent->left == delete) {
		parent->left = child;
		direct = -1;
	}
	else {
		parent->right = child;
		direct = 1;
	}
	if (child)
		child->parent = parent;
	delete_fixup_bintree3(ptr, parent, child, direct);
	free_single_binnode3(delete);
}

static void delete_swap_binnode3(bintree3 *ptr, binnode3 *replace)
{
	binnode3 *delete, *right;

	delete = next_binnode3(replace);
	right = delete->right;
	delete_copy_binnode3(replace, delete);
	delete_single_binnode3(ptr, delete, right);
}

static void delete_binnode3(bintree3 *ptr, binnode3 *delete)
{
	binnode3 *left, *right;

	left = delete->left;
	right = delete->right;
	if (left && right)
		delete_swap_binnode3(ptr, delete);
	else if (left)
		delete_single_binnode3(ptr, delete, left);
	else
		delete_single_binnode3(ptr, delete, right);
}

int delete_bintree3(bintree3 *ptr, binkey3 key, int *ret)
{
	binnode3 *delete;

	if (search_binnode3(ptr, key, &delete))
		return 1;
	if (delete == NULL) {
		*ret = 0;
		return 0;
	}
	delete_binnode3(ptr, delete);
	ptr->size--;
	*ret = 1;
	return 0;
}

