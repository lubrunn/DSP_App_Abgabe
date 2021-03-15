##### in this file we create the description of the network plot



network_description_text <- "<div style='background-color:#344a5f; padding: 20px'>

<head>
<h4>Network Plot</h4>


In this section we aim to analyse word networks. We offer both bigrams and word pairs as possible
word combinations to analyse. Bigrams are consecutive word combinations. For exmaple the tweet: 'it is late' would create the bigrams:
'it is' and 'is late'. Word pairs are all possible word combinations in a tweet. The tweet from above would create the word pairs: 'it is',
'is late', 'it late'. Hence, there are considerably more word pairs than bigrams for a given sentence. The advantage of word pairs is
that one also finds word combinations that often appear together even if they are not followed by each other. <br/>
<br/>
The word network then depicts all unique word combinations found. The network consists of nodes and links. A node is a single
word and a link connects two words. A node is created for every single unique word and links are created for
every single unique word combination. For example if the data contained the three word combinations 'this is' 'is an' and 'example text', 5 nodes
and 3 links would be created. For further information you may visit <a href='https://www.tidytextmining.com/ngrams.html'>Tidytextmining</a> or
<a href='https://cbail.github.io/SICSS_Text_Networks.html'>Text Networks</a> .
<br/>
<br/>
In this application the size of the nodes increase with the number of adjacent links it has. For bigrams the link width depends on the number of
occurences of an unque bigram. For the the word pairs the link width increases with an increasing word correlation.
The word correlation is a measure that depicts how often words either appear together or
not at all compared to appearing alone. It helps indentifying word combinations that often appear together while ignoring the total number
of occurences. For more information you may visit <a href='https://www.tidytextmining.com/ngrams.html'>Tidytextmining</a> .
<br/>
<br/>
Note that the computation of the word network may take some time, hence, we only allow for the analysis of a maximum of 2 days.
However, you may search more precisely for tweets containing specific words or for tweets from specific users.
Once started, the computation cannot be started again until the word network
has been fully computed. But you can cancel the running process in case you want to change parameters without waiting for the current
computation to be done. The networks can appear
overcrowed if low threshold filters (minimum number of occurences / minimum correlatioin) are chosen which may lead to lag. Hence, we only
show a maximum of 2000 unique word combinations in a network plot. The thresholds for minimum number of occurences are set to 10 while the minimum
correlation is set to 0.15. When choosing lower values the filters will default to the aforementioned values.
If the plot still becomes too overcrowded we firstly recommend removing the network through the 'Remove Network' button in order to avoid lag.
Then the bigram networks tend be less overcrowded in general. One can then also increase the minimum thresholds until the plot becomes cleaerer.
All filters can be accessed the dropdown menu in the upper left corner.
<br/>
<br/>
Below the network plot you can already take a look at the raw tweets for you current filter selection. Note that the table updated immediately when changing inputs and
not only after presssing the button. This may be a good total to see the number of available tweets for a specific filter selection.


</div>


"
