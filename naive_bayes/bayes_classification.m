clear

% read vocab to get size
vid = fopen('vocabulary.txt');
vocabulary = textscan(vid, '%s');
fclose(vid);

%loading the classes
cid = fopen('newsgrouplabels.txt');
classes_raw = textscan(cid, '%s');
fclose(cid);

train_instances = dlmread('train.data');
train_labels = dlmread('train.label');

test_instances = dlmread('test.data');
test_label = dlmread('test.label');

classes = classes_raw{1};
number_of_classes = length(classes);
vocab = vocabulary{1};
vocab_size = length(vocab);
betas = [0.000001 0.00001 0.0001 0.001 0.01 0.1 0.5 1];

mle_counts = histc(train_labels, 1:number_of_classes);
mle_counts = mle_counts ./ sum(mle_counts);
MLE = @(x) mle_counts(x); 

for b = 1 : length(betas)
    beta = betas(b);
    
    probabilities = sparse(train_labels(train_instances(:,1)), train_instances(:,2), train_instances(:,3), number_of_classes, vocab_size);

    count_by_class = @(y) probabilities(y, :);

    MAP_vector = @(y) (count_by_class(y) + beta) ./ (sum(count_by_class(y)) + (beta * vocab_size));

    confusion_matrix = zeros(20, 20);

    test_matrix = full(sparse(test_instances(:,1), test_instances(:, 2), test_instances(:, 3)));

    [m,n] = size(test_matrix);

    c = zeros(m,1);
    fprintf('starting classification for beta=%f\n', beta);
    
    for i = 1 : m
        max = -10000000;
        for y = 1 : number_of_classes
            val = log2(MLE(y)) + log2(MAP_vector(y)) * test_matrix(i, :)';

            if val > max
                max = val;
                c(i) = y;
            end
        end
%        if 0 == mod(i, 100)
%            fprintf('Finished classifying %d\n', i);
%        end
    end
    fprintf('finished classification\n')

    for i = 1 : length(c)
        confusion_matrix(test_label(i), c(i)) = confusion_matrix(test_label(i), c(i)) + 1;
    end

    %confusion_matrix(test_label, c) = confusion_matrix(test_label, c) + 1;
    acc = 0;
    for i = 1 : number_of_classes
        acc = acc + confusion_matrix(i, i);
    end

%    beta
%    confusion_matrix
    fprintf('accuracy (�= %f): %f\n', beta, acc / m);

end
