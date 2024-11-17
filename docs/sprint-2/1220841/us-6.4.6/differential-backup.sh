#!/bin/bash
# Differential backup script
DATE=$(date +%Y-%m-%d)
BACKUP_DIR="/backup/differential"

# Check if the backup directory exists, and create it only if it does not
if [ ! -d "$BACKUP_DIR" ]; then
  mkdir -p "$BACKUP_DIR"
fi

# Perform the differential backup and compress it
mysqldump -h vsgate-s1.dei.isep.ipp.pt -P 11433  -u root -p'PF+w2gYZ+0Wz' --all-databases --single-transaction --flush-logs --skip-lock-tables > $BACKUP_DIR/diff_backup_$DATE.sql
tar -czf $BACKUP_DIR/diff_backup_$DATE.tar.gz -C $BACKUP_DIR diff_backup_$DATE.sql

# Remove the uncompressed SQL file to save space
rm $BACKUP_DIR/diff_backup_$DATE.sql
