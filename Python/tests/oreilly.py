#!/usr/bin/env python3
# Object testing:

# custom iterable testing
import collections

Card = collections.namedtuple('Card', ['rank', 'suit'])


class Deck:
    ranks = [str(n) for n in range(2, 11)] + list('JKQA')
    suits = {"clubs": 0, "diamonds": 1, "hearts": 2, "spades": 3}

    def __init__(self):
        self._cards = [Card(rank, suit)
                       for rank in self.ranks
                       for suit in self.suits.keys()]

    # len(self) response
    def __len__(self):
        return len(self._cards)

    # Index operator
    def __getitem__(self, position):
        return self._cards[position]


def spades_high(card):
    rank_val = Deck.ranks.index(card.rank)
    return rank_val * len(Deck.suits) + Deck.suits[card.suit]


test = Deck()
print('slicing/access:')
print(test[:3])

print('\nsorted:')
for card in sorted(test, key=spades_high):
    print(card)
