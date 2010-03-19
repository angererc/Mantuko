% @type split_node(ActivationOptions)
% 	ActivationOptions = set(values:activation_option()).
-record (split_node, {activation_options}).

-record (union_node, {}).

% @type atom_node(ActivationOption)
%	ActivationOption = refs:activation_option().
-record (atom_node, {activation_option}).