// SPDX-License-Identifier: GPL-2.0
/*
 * Copyright (C) 2020 Bootlin
 *
 * Author: Joao Marcos Costa <joaomarcos.costa@bootlin.com>
 */

#include <errno.h>
#include <linux/types.h>
#include <linux/byteorder/little_endian.h>
#include <linux/byteorder/generic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "sqfs_filesystem.h"
#include "sqfs_utils.h"

bool sqfs_is_dir(__le16 type)
{
	return (le16_to_cpu(type) == SQFS_DIR_TYPE) ||
		(le16_to_cpu(type) == SQFS_LDIR_TYPE);
}

/*
 * Receives a pointer (void *) to a position in the inode table containing the
 * directory's inode. Returns directory inode offset into the directory table.
 * m_list contains each metadata block's position, and m_count is the number of
 * elements of m_list. Those metadata blocks come from the compressed directory
 * table.
 */
int sqfs_dir_offset(void *dir_i, u32 *m_list, int m_count)
{
	struct squashfs_base_inode base;
	struct squashfs_ldir_inode ldir;
	struct squashfs_dir_inode dir;
	u32 start_block;
	u16 offset;
	int j;

	memcpy(&base, dir_i, sizeof(base));

	switch (base.inode_type) {
	case SQFS_DIR_TYPE:
		memcpy(&dir, dir_i, sizeof(dir));
		start_block = le32_to_cpu(dir.start_block);
		offset = le16_to_cpu(dir.offset);
		break;
	case SQFS_LDIR_TYPE:
		memcpy(&ldir, dir_i, sizeof(ldir));
		start_block = le32_to_cpu(ldir.start_block);
		offset = le16_to_cpu(ldir.offset);
		break;
	default:
		printf("Error: this is not a directory.\n");
		return -EINVAL;
	}

	for (j = 0; j < m_count; j++) {
		if (m_list[j] == start_block)
			return (++j * SQFS_METADATA_BLOCK_SIZE) + offset;
	}

	if (start_block == 0)
		return offset;

	printf("Error: invalid inode reference to directory table.\n");

	return -EINVAL;
}

bool sqfs_is_empty_dir(void *dir_i)
{
	struct squashfs_base_inode *base;
	struct squashfs_ldir_inode *ldir;
	struct squashfs_dir_inode *dir;
	u32 file_size;

	base = malloc(sizeof(*base));
	if (!base)
		return errno;

	memcpy(base, dir_i, sizeof(*base));

	switch (le16_to_cpu(base->inode_type)) {
	case SQFS_DIR_TYPE:
		dir = malloc(sizeof(*dir));
		memcpy(dir, dir_i, sizeof(*dir));
		file_size = le16_to_cpu(dir->file_size);
		free(dir);
		break;
	case SQFS_LDIR_TYPE:
		ldir = malloc(sizeof(*ldir));
		memcpy(ldir, dir_i, sizeof(*ldir));
		file_size = le32_to_cpu(ldir->file_size);
		free(ldir);
		break;
	default:
		printf("Error: this is not a directory.\n");
		free(base);
		return false;
	}

	free(base);

	return file_size == SQFS_EMPTY_FILE_SIZE;
}
