function update_process_traits_TIE(trait_name, base_directory, data_range, prefix, matlab_script_path)
    % Save the current directory to restore it later
    original_dir = pwd;
    
    % Process traits based on the provided parameters
    % Create the new folder name for output files
    new_folder = fullfile(base_directory, [prefix, '_results']);
    
    % Create the new folder if it does not exist
    if ~exist(new_folder, 'dir')
        mkdir(new_folder);
    end
    
    % Loop through all subgroups and iterations
    for subgroup = 1:20
        for iteration = 1:30
            % Construct the file name
            file_name = sprintf('tie_ranked_%s_subgroup_%d_iteration_%d.csv', trait_name, subgroup, iteration);
            file_path = fullfile(base_directory, file_name);

            % Define output file names within the new folder
            output_files = {
                sprintf('%s_A_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration),
                sprintf('%s_mu_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration),
                sprintf('%s_ams_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration),
                sprintf('%s_bcs_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration),
                sprintf('%s_logs_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration),
                sprintf('%s_sbs_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration)
            };

            % Check if all output files exist and have the correct dimensions
            rerun_needed = false;
            for output_file = output_files
                output_file_path = fullfile(new_folder, output_file{1});
                if ~isfile(output_file_path)
                    rerun_needed = true;
                    break;
                else
                    data = readmatrix(output_file_path);
                    if size(data, 1) ~= 3000 || size(data, 2) ~= 1
                        rerun_needed = true;
                        break;
                    end
                end
            end

            if ~rerun_needed
                disp(['All output files already exist with correct dimensions for trait ' trait_name ', subgroup ' num2str(subgroup) ', iteration ' num2str(iteration) '. Skipping processing.']);
                continue;
            end

            % Import the data as a numeric matrix
            A_matrix = readmatrix(file_path, 'Range', data_range, 'OutputType', 'double');

            % Calculate dimensions and missing data
            nusers = size(A_matrix, 1);
            nitems = size(A_matrix, 2);
            nmissing = numel(A_matrix) - nnz(A_matrix);
            minitemratings = min(sum(spones(A_matrix)));
            minuseratings = min(sum(spones(A_matrix), 2));

            % Change directory to the specified MATLAB script path
            cd(matlab_script_path);

            % Calculate statistical measures
            mu = mean_rating(A_matrix);
            ams = ssmcr(A_matrix);
            bcs = ssmcr(A_matrix, 'skewtype', 'bc');
            logs = ssmcr(A_matrix, 'skewtype', 'lo');
            sbs = ssmcr(A_matrix, 'skewtype', 'sb');

            % Double-check the dimensions of the results
            if size(mu, 1) ~= 3000 || size(mu, 2) ~= 1 || ...
               size(ams, 1) ~= 3000 || size(ams, 2) ~= 1 || ...
               size(bcs, 1) ~= 3000 || size(bcs, 2) ~= 1 || ...
               size(logs, 1) ~= 3000 || size(logs, 2) ~= 1 || ...
               size(sbs, 1) ~= 3000 || size(sbs, 2) ~= 1
                error(['Incorrect output size for trait ' trait_name ', subgroup ' num2str(subgroup) ', iteration ' num2str(iteration) '. Expected 3000x1.']);
            end

            % Save data to CSV files, naming files dynamically within the new folder
            csvwrite(fullfile(new_folder, sprintf('%s_A_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration)), A_matrix);
            csvwrite(fullfile(new_folder, sprintf('%s_mu_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration)), mu);
            csvwrite(fullfile(new_folder, sprintf('%s_ams_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration)), ams);
            csvwrite(fullfile(new_folder, sprintf('%s_bcs_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration)), bcs);
            csvwrite(fullfile(new_folder, sprintf('%s_logs_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration)), logs);
            csvwrite(fullfile(new_folder, sprintf('%s_sbs_%s_subgroup%d_iteration%d.csv', prefix, trait_name, subgroup, iteration)), sbs);

            % Display a message to indicate successful processing
            disp(['Processed and saved results for trait ' trait_name ', subgroup ' num2str(subgroup) ', iteration ' num2str(iteration)]);
        end
    end
    
    % Change back to the original directory
    cd(original_dir);
end
