def topn(listlike, n):
    """Get the indices and values of the highest n of a list"""
    indices = sorted(range(len(listlike)),
                     key=lambda i: listlike[i],
                     reverse=True)[:n]
    values = [listlike[i] for i in indices]
    return indices, values


def bottomn(listlike, n):
    """Get the indices and values of the lowest n of a list"""
    indices = sorted(range(len(listlike)), 
                     key=lambda i: listlike[i],
                     reverse=False)[:n]
    values = [listlike[i] for i in indices]
    return indices, values


def flatten(list_of_lists):
    """Change a list of lists to one big list"""
    return [item for sublist in list_of_lists for item in sublist]


def indexify(listlike):
    """Create dictionaries of ix <-> item in an iterable"""
    item2ix = { item:i for i,item in enumerate(listlike) }
    ix2item = { i:item for i,item in enumerate(listlike) }
    return item2ix, ix2item


def digits_to_int(tup):
    """Given a list of digits, return the corresponding integer"""
    result = 0
    for i in range(len(tup)):
        result += tup[len(tup)-1-i] * 10**i
    return result
    

def is_palindrome(input):
    """Return True if the input sequence is palindromic"""
    hlen = len(input)//2
    for x in range(hlen):
        if input[x] != input[-x-1]:
            return(False)
    return(True)