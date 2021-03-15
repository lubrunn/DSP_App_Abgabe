network_description_text <- "<div style='background-color:#344a5f; padding: 20px'>

<head>
<h4>Description Network Plot</h4>


In this section we aim to analyse word networks. We offer both bigrams and word pairs as possible
word combinations to analyse. Bigrams are consecutive word combinations. For exmaple the tweet: 'it is late' would create the bigrams:
'it is', 'is late'.Word pairs are all possible word combinations in a tweet. The tweet from above would create the word pairs: 'it is',
'is late', 'it late'. Hence, there are considerably more word pairs than bigrams for a given sentence. The advantage of word pairs is
that one also finds word combinations that often appear together even if they are not followed by each other. <br/>
<br/>
The word network then depicts all unique word combinations found. The network consists of nodes and links. A node is a single
word and a link connects two words. A node is created for every single unique word and links are created for
every single unique word combination. For example if there were three bigrams in the data: 'this is' 'is an' 'example text' 5 nodes
and 3 links would be created. For further information consult: <a href='https://www.tidytextmining.com/ngrams.html'>Tidytextmining</a> or
<a href='https://cbail.github.io/SICSS_Text_Networks.html'>Text Networks</a> .
<br/>
<br/>
In this application the size of the nodes increases with the number of adjacent links it has and links become thicker the more often a bigram appears.
For word pairs
the link width corresponds to the word correlation. The word correlation is a measure that depicts how often words either appear together or
not at all compared to appearing alone. For more information you may consult: <a href='https://www.tidytextmining.com/ngrams.html'>Tidytextmining</a> .
<br/>
<br/>
Note that the computation of the word network may take some time, hence, we allow for cancelling the process. The networks may appear
overcrowed if low threshold filter are chosen which may lead to lag. Therefore, you can also remove the plot after creation. One way
to downsize a network plot is through increasing the minimum number of occurences a single word needs to have and by increasing the
minimum number of occurences of a bigram / the correaltion of word pairs. Also note that this analysis is restricted to a maximum date
range of 5 days. However, you may search more precisely for tweets containing specific words or for tweets from specific users.

</div>


"
