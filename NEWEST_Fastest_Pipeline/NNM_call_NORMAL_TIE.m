% Example usage for each trait
matlab_script_path = '/n/holyscratch01/duan_lab/jiaxin/skew-nuclear/matlab';

% Process BMI
update_process_traits_TIE('bmi', '/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/2.2_bmi_TIE_ranked_transposed_files/', 'A2:DKJ53', 'NORMAL_TIE_NNM', matlab_script_path);

% Process Height
update_process_traits_TIE('height', '/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/2.2_height_TIE_ranked_transposed_files/', 'A2:DKJ73', 'NORMAL_TIE_NNM', matlab_script_path);

% Process BRC
update_process_traits_TIE('brc', '/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/2.2_brc_TIE_ranked_transposed_files/', 'A2:DKJ109', 'NORMAL_TIE_NNM', matlab_script_path);
