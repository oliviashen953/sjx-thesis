% 2.4_NORMAL_NNM_HEIGHT
% Define the base directory where the ranked transposed files are located
base_directory = '/n/home00/jiaxinshen/NORMAL/2.2_height_ranked_transposed_files/';

% Define the range to be read from each CSV file
data_range = 'A2:DKJ73';  % Assuming 3000 columns, the range is A2:DK3001 for 72 rows (1 header row)

% Define the new folder name under the base path for output files
new_folder = 'NORMAL_new_height_results';

% Create the new folder if it does not exist
csv_base_path = '/n/home00/jiaxinshen/skew-nuclear/matlab/';
new_folder_path = fullfile(csv_base_path, new_folder);
if ~exist(new_folder_path, 'dir')
    mkdir(new_folder_path);
end

% Loop through all subgroups and iterations
for subgroup = 1:20
    for iteration = 1:30
        % Construct the file name
        file_name = sprintf('ranked_transposed_height_subgroup_%d_iteration_%d.csv', subgroup, iteration);
        file_path = fullfile(base_directory, file_name);

        % Define output file names within the new folder
        output_files = {
            sprintf('NORMAL_new_A_height_subgroup%d_iteration%d.csv', subgroup, iteration),
            sprintf('NORMAL_new_mu_height_subgroup%d_iteration%d.csv', subgroup, iteration),
            sprintf('NORMAL_new_ams_height_subgroup%d_iteration%d.csv', subgroup, iteration),
            sprintf('NORMAL_new_bcs_height_subgroup%d_iteration%d.csv', subgroup, iteration),
            sprintf('NORMAL_new_logs_height_subgroup%d_iteration%d.csv', subgroup, iteration),
            sprintf('NORMAL_new_sbs_height_subgroup%d_iteration%d.csv', subgroup, iteration)
        };

        % Check if all output files exist and have the correct dimensions
        rerun_needed = false;
        for output_file = output_files
            output_file_path = fullfile(new_folder_path, output_file{1});
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
            disp(['All output files already exist with correct dimensions for subgroup ' num2str(subgroup) ', iteration ' num2str(iteration) '. Skipping processing.']);
            continue;
        end

        % Import the data as a numeric matrix
        A_height_matrix = readmatrix(file_path, 'Range', data_range, 'OutputType', 'double');

        % Calculate dimensions and missing data
        nusers = size(A_height_matrix, 1);
        nitems = size(A_height_matrix, 2);
        nmissing = numel(A_height_matrix) - nnz(A_height_matrix);
        minitemratings = min(sum(spones(A_height_matrix)));
        minuseratings = min(sum(spones(A_height_matrix), 2));

        % Change directory to the specified MATLAB script path
        cd('/n/home00/jiaxinshen/skew-nuclear/matlab');

        % Calculate statistical measures
        mu = mean_rating(A_height_matrix);
        ams = ssmcr(A_height_matrix);
        bcs = ssmcr(A_height_matrix, 'skewtype', 'bc');
        logs = ssmcr(A_height_matrix, 'skewtype', 'lo');
        sbs = ssmcr(A_height_matrix, 'skewtype', 'sb');

        % Double-check the dimensions of the results
        if size(mu, 1) ~= 3000 || size(mu, 2) ~= 1 || ...
           size(ams, 1) ~= 3000 || size(ams, 2) ~= 1 || ...
           size(bcs, 1) ~= 3000 || size(bcs, 2) ~= 1 || ...
           size(logs, 1) ~= 3000 || size(logs, 2) ~= 1 || ...
           size(sbs, 1) ~= 3000 || size(sbs, 2) ~= 1
            error(['Incorrect output size for subgroup ' num2str(subgroup) ', iteration ' num2str(iteration) '. Expected 3000x1.']);
        end

        % Save data to CSV files, naming files dynamically within the new folder
        csvwrite(fullfile(new_folder_path, sprintf('NORMAL_new_A_height_subgroup%d_iteration%d.csv', subgroup, iteration)), A_height_matrix);
        csvwrite(fullfile(new_folder_path, sprintf('NORMAL_new_mu_height_subgroup%d_iteration%d.csv', subgroup, iteration)), mu);
        csvwrite(fullfile(new_folder_path, sprintf('NORMAL_new_ams_height_subgroup%d_iteration%d.csv', subgroup, iteration)), ams);
        csvwrite(fullfile(new_folder_path, sprintf('NORMAL_new_bcs_height_subgroup%d_iteration%d.csv', subgroup, iteration)), bcs);
        csvwrite(fullfile(new_folder_path, sprintf('NORMAL_new_logs_height_subgroup%d_iteration%d.csv', subgroup, iteration)), logs);
        csvwrite(fullfile(new_folder_path, sprintf('NORMAL_new_sbs_height_subgroup%d_iteration%d.csv', subgroup, iteration)), sbs);

        % Display a message to indicate successful processing
        disp(['Processed and saved results for subgroup ' num2str(subgroup) ', iteration ' num2str(iteration)]);
    end
end