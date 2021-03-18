##### in this file we create the description of the network plot



network_description_text <- "<div style='background-color:#344a5f; padding: 20px'>

<head>
<h4>Network Plot</h4>


In this section we aim to analyse word networks. We offer both bigrams and word pairs as possible
word combinations to analyse. Bigrams are consecutive word combinations. For example, the tweet: 'it is late' would create the bigrams:
'it is' and 'is late'. Word pairs are all possible word combinations in a tweet. The tweet from above would create the word pairs: 'it is',
'is late' and 'it late'. Hence, there are considerably more word pairs than bigrams for a given sentence. The advantage of word pairs is
that one also finds word combinations that often appear together even if they are not followed by each other. <br/>
<br/>
The word network then depicts all unique word combinations found. The network consists of nodes and links. A node is a single
word and a link connects two words. A node is created for every single unique word and links are created for
every single unique word combination. For example, if the data contained the three word combinations 'this is' 'is an' and 'example text', 5 nodes
and 3 links would be created. For further information you may visit <a href='https://www.tidytextmining.com/ngrams.html'>Tidytextmining</a> or
<a href='https://cbail.github.io/SICSS_Text_Networks.html'>Text Networks</a> .
<br/>
<br/>
In this application the size of the nodes increase with the number of adjacent links it has. For bigrams the link width depends on the number of
occurrences of an unique bigram. For the word pairs the link width increases with an increasing word correlation.
The word correlation is a measure that depicts how often words either appear together or
not at all compared to appearing alone. It helps identifying word combinations that often appear together while ignoring the total number
of occurences. For more information you may visit <a href='https://www.tidytextmining.com/ngrams.html'>Tidytextmining</a> .
<br/>
<br/>
Note that the computation of the word network may take some time, hence, we only allow for the analysis of a maximum of 2 days.
However, you may search more precisely for tweets containing specific words or for tweets from specific users.
Once started, the computation cannot be started again until the word network
has been fully computed. But you can cancel the running process in case you want to change parameters without waiting for the current
computation to be done. The networks can appear very empty when reducing the data through the use of filters. In that case you can adjust
the minimum threshold for occurences and correlation using the advanced settings.
However, note that setting values too low may result in very overcrowded plots. Hence, we only
show a maximum of 2000 unique word combinations in a network plot.


For most filter options the absolute thresholds for minimum number of
occurrences are set to the maximum of 10 and 0.1% of the number of tweets found.
For the word correlation the minimum
correlation is set to 0.15. However, don't worry too much about this as the filters will automatically
default to the aforementioned values when choosing lower
values than the absoulte thresholds.





The username search is an
exception here as it will results in considerably fewer tweets,
Here the minimum number of occurences can be set as low as 1 and
the minimum correlation can be
set to 0.1.
If the plot still becomes too overcrowded we recommend to first remove the network through the 'Remove Network' button in order to avoid lag.
Then we recommend increasing the thresholds. Also, usually bigram networks tend be less overcrowded compared to word pair networks.
<br/>
<br/>
Below the network plot you can also take a look at the raw tweets for your current filter selection. Note that the table updates immediately
when changing inputs and
not only after pressing the button. This provides the possibility to scan the data before starting the computation.


</div>


"
