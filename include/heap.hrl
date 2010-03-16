
-record (struct, {}).

-record (array, {}).

-record (lock, {}).

% @type branch_in_node(ActivationOptions)
% 	ActivationOptions = set(values:activation_option()).
-record (branch_in_node, {activation_options}).

-record (branch_out_node, {}).

% @type option_node(ActivationOption)
%	ActivationOption = values:activation_option().
-record (option_node, {activation_option}).
