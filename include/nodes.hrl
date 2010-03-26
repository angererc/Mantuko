%the nodes hierarchy; you should not use those directly, they are considered private by the node.erl and related modules
-record (split_node, {closures}).

-record (union_node, {}).

-record (atom_node, {closure}).