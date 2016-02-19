Notes:

1. Current method using basic threads and analyzing the graph.
2. Future based approach. Unclear how to make “data stores”
3. Actor based approach. Send messages down the graph. Wait for responses seems to work for preventing race conditions.

Things that need to be in place for us to do some testing.
DataElement - Immutable wrapper around an Array[Double] (indexedSeq)
Output/Input is a Vector[DataElement]
Need sources, a few filters, and a sink

