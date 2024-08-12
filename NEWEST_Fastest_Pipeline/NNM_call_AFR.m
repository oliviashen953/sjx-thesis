% Example usage for each trait
matlab_script_path = '/n/holyscratch01/duan_lab/jiaxin/skew-nuclear/matlab';

% Process BMI
update_process_traits('bmi', '/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/2.2_bmi_ranked_transposed_files/', 'A2:DKJ53', 'NORMAL_NNM', matlab_script_path);

% Process Height
update_process_traits('height', '/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/2.2_height_ranked_transposed_files/', 'A2:DKJ73', 'NORMAL_NNM', matlab_script_path);

% Process BRC
update_process_traits('brc', '/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/2.2_brc_ranked_transposed_files/', 'A2:DKJ109', 'NORMAL_NNM', matlab_script_path);
