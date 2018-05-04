module Hit.Snapshot.Data where

import Hit.Objects

data SnapshotEntry = BlobEntry Blob | TreeEntry [SnapshotEntry]

data Snapshot = Snapshot{
    tree :: SnapshotEntry
}