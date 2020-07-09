// SPDX-License-Identifier: GPL-2.0
/*
 * Copyright (C) 2020 Bootlin
 *
 * Author: Joao Marcos Costa <joaomarcos.costa@bootlin.com>
 */

#include <asm/unaligned.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqfs_decompressor.h"
#include "sqfs_filesystem.h"
#include "sqfs_utils.h"

/*
 * Given the uncompressed inode table, the inode to be found and the number of
 * inodes in the table, return inode position in case of success.
 */
void *sqfs_find_inode(void *inode_table, int inode_number, __le32 inode_count,
		      __le32 block_size)
{
	int k, l, blk_list_size = 0, offset = 0, index_list_size = 0;
	struct squashfs_symlink_inode symlink;
	struct squashfs_base_inode base;
	struct squashfs_ldev_inode ldev;
	struct squashfs_lreg_inode lreg;
	struct squashfs_ldir_inode ldir;
	struct squashfs_lipc_inode lipc;
	struct squashfs_dev_inode dev;
	struct squashfs_reg_inode reg;
	struct squashfs_dir_inode dir;
	struct squashfs_ipc_inode ipc;

	if (!inode_table) {
		printf("%s: Invalid pointer to inode table.\n", __func__);
		return NULL;
	}

	for (k = 0; k < le32_to_cpu(inode_count); k++) {
		memcpy(&base, inode_table + offset, sizeof(base));
		if (le32_to_cpu(base.inode_number) == inode_number)
			return (void *)(inode_table + offset);

		switch (base.inode_type) {
		case SQFS_DIR_TYPE:
			memcpy(&dir, inode_table + offset, sizeof(dir));
			offset += sizeof(dir);
			break;
		case SQFS_REG_TYPE:
			memcpy(&reg, inode_table + offset, sizeof(reg));
			if (SQFS_IS_FRAGMENTED(reg.fragment)) {
				blk_list_size = (reg.file_size / block_size);
			} else {
				blk_list_size = DIV_ROUND_UP(reg.file_size,
							     block_size);
			}

			offset += sizeof(reg) + blk_list_size * sizeof(u32);
			break;
		case SQFS_LDIR_TYPE:
			memcpy(&ldir, inode_table + offset, sizeof(ldir));
			if (ldir.i_count == 0) {
				offset += sizeof(ldir);
				break;
			}

			for (l = 0; l < ldir.i_count + 1; l++)
				index_list_size += ldir.index[l].size + 1;

			offset += sizeof(ldir) + index_list_size +
				(ldir.i_count + 1) * SQFS_DIR_INDEX_BASE_LENGTH;
			index_list_size = 0;
			break;
		case SQFS_LREG_TYPE:
			memcpy(&lreg, inode_table + offset, sizeof(lreg));
			if (lreg.fragment == 0xFFFFFFFF) {
				blk_list_size = DIV_ROUND_UP(lreg.file_size,
							     block_size);
			} else {
				blk_list_size = (lreg.file_size / block_size);
			}

			offset += sizeof(lreg) + blk_list_size * sizeof(u32);
			break;
		case SQFS_SYMLINK_TYPE:
		case SQFS_LSYMLINK_TYPE:
			memcpy(&symlink, inode_table + offset, sizeof(symlink));
			offset += sizeof(symlink) + symlink.symlink_size;
			break;
		case SQFS_BLKDEV_TYPE:
		case SQFS_CHRDEV_TYPE:
			memcpy(&dev, inode_table + offset, sizeof(dev));
			offset += sizeof(dev);
			break;
		case SQFS_LBLKDEV_TYPE:
		case SQFS_LCHRDEV_TYPE:
			memcpy(&ldev, inode_table + offset, sizeof(ldev));
			offset += sizeof(ldev);
			break;
		case SQFS_FIFO_TYPE:
		case SQFS_SOCKET_TYPE:
			memcpy(&ipc, inode_table + offset, sizeof(ipc));
			offset += sizeof(ipc);
			break;
		case SQFS_LFIFO_TYPE:
		case SQFS_LSOCKET_TYPE:
			memcpy(&lipc, inode_table + offset, sizeof(lipc));
			offset += sizeof(lipc);
			break;
		default:
			printf("Error while searching inode: unknown type.\n");
			return NULL;
		}
	}

	printf("Inode not found.\n");

	return NULL;
}

int sqfs_read_metablock(unsigned char *file_mapping, int offset,
			bool *compressed, u32 *data_size)
{
	unsigned char *data;
	u16 header;

	data = file_mapping + offset;
	header = get_unaligned((u16 *)data);
	*compressed = SQFS_COMPRESSED_METADATA(header);
	*data_size = SQFS_METADATA_SIZE(header);

	if (*data_size > SQFS_METADATA_BLOCK_SIZE) {
		printf("Invalid metatada block size: %d bytes.\n", *data_size);
		return -EINVAL;
	}

	return 0;
}
