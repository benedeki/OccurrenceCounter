# Occurrence Counter
OccurrenceCounter is generic class that serves the purpose of continuously counting the number of occurrences of item 
kinds. The class is able to return the top and bottom items at any moment; top being the items with the highest 
occurrence count, bottom with the lowest.

The adding of an item to the counter is done in constant time, therefore adding a sequence of items is cost linear time
in the context of sequence length.
Getting the bottom and top items is effectively equal to creating a sequence of the given size. 
