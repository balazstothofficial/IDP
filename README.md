# IDP

An implementation of the LDA Topic Modelling algorithm in Haskell.
The implementation is generic for all text-based documents. However, the goal of this project was to run the algorithm for qualitative interviews, on which the naming schema is based.

[Main](app/Main.hs)

Entry point of the program. Using the different `run` functions from `InterviewLDARunner`, the LDA-alogrithm can be run on the data set specified in `Configuration`.

[InterviewLDARunner](src/InterviewLDARunner.hs)

* `runLDAOnFullDataSet`: Run the algorithm on the full data set defined in `Configuration`. Every file is represent as a separate document.
* `runLDAPerSubfolder`: Run the algorithm in parallel for each subfolder of the data set defined in `Configuration`. Every file is represent as a separate document.
* `runLDAPerCombinedSubfolder`: Run the algorithm in parallel for each subfolder of the data set defined in `Configuration`. Every subfolder is represent as one combined document.
* `runLDAOnCombinedFullDataSet`: Run the algorithm on the full data set defined in `Configuration`. All files are combined to a single document.
* `runLDAOnFullDataSetWithCombinedSubfolders`: Run the algorithm on the full data set defined in `Configuration`. Every subfolder is combined to a document.
* `fullDataSet`: Defines the name of the result file, if the whole data set is combined to a single file.

[Configuration](src/Configuration.hs)

* `interviewDirectory`: Configures the location of the input data.
* `resultDirectory`: Configures the output location.
* `seed`: Seed for the random number generator on which the algorithm is based.
* `numberOfTopics`: Configures how many different topics should be considered.
* `saveIterations`: Configures how many iterations of the `saveInterval` should be executed.
* `saveInterval`: Configures how many iterations should be executed before a (intermediate) result is stored.

[LDARunner](src/LDARunner.hs)

* `LDARunner`: Type class for running the LDA-algorithm on a specified input.
* `Input`: Standard input of the algorithm.
* `instance LDARunner Input`: Runs the algorithm on the configuration parameters provided in `Input`.

[LDAEstimator](src/LDAEstimator.hs)

Main logic of the LDA-algorithm. 

* `Input`: Input parameter for the estimation algorithm.
* `estimate`: Returns a trained `Model` based on the parameters of `Input`.

[LDAFinalizer](src/LDAFinalizer.hs)

Computes the result matrices after the `LDAEstimator` finished the training of the `Model`. The performance would suffer a lot if this would be executed after each iteration. Is executed when a output should be written.

[Directory](src/Directory.hs)

Helper functions to work with absolute and relative file and directory paths.

[Document](src/Document.hs)

Data structre for a single document of the `Model`. Contains a document `title`, all the `words` of the document and the counts of the single words.

* `instance Factory Interview Document`: A `Document` can be created with this `Factory` out of an `Interview`.
* `stopWords`: Configuration of stop words, which are filtered out when creating `Document`s. Based on [this list of german stop words.](https://countwordsfree.com/stopwords/german)

[DocumentTopicMap](src/DocumentTopicMap.hs)

Keeps track of how many times a `Topic` occurs in a `Document`.

[Factory](src/Factory.hs)

Syntatic sugar for factory functions.

[HyperParameter](src/HyperParameter.hs)

Hyperparameters to influence training. Default values are:

* alpha = 50 / no of topics
* beta = 0.1

[IntitialTopics](src/IntitialTopics.hs)

Intital (ideally random) topic distribution of the words in the data set.

[Interview](src/Interview.hs)

Data structure to represent read input interview before further processing.

[InterviewReader](src/InterviewReader.hs)

Read interviews from provided path.

[List](src/List.hs)

Helper list functions.

[Model](src/Model.hs)

Represetation of the state of the training. Should be created using the `ModelFactory`.

[ModelFactory](src/ModelFactory.hs)

Create an intital `Model` out of the provided `Input` parameters.

[TopicAssignments](src/TopicAssignments.hs)

Stores the assignements of the topics in matrix form (Documents x Words).

[TopicCounts](src/TopicCounts.hs)

Stores the how often the topics are assigned to words of the data set.

[Vocabulary](src/Vocabulary.hs)

Set of all words contained in the data set.

[WordTopicMap](src/WordTopicMap.hs)

Keeps track of how many times a topic is assigned to a word.