%private to the object hierarchy; don't use those records directly, if possible
-record (struct, {reader, writer, slots}).

-record (array, {reader, writer, values}).

-record (lock, {reader, writer}).