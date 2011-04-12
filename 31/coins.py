def coin_poss(coins, amount, result):
    k = str(amount)
    if k not in result:
        result[k] = sum([coin_poss(coins, amount - x, result) 
                        for x in coins if x <= amount])
    return result[k]

