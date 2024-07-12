









def accuracy(matching, answers):
    total = 0 
    right = 0 
    for i in answers:
        if((i, answers[i]) in matching or (answers[i],i)in matching):
            right += 1 
        total += 1
    return right/total