#!/usr/bin/env python

"""
Python source code - replace this with a description of the code and write the code below this text.
"""

def load_vocab():
    """docstring for load_vocab"""

    vocab = []
    with open('./vocab.txt', 'r') as reader:
        for line in reader:
            vocab.append(line.strip())

    vocab = dict([(word, idx) for idx, word in enumerate(vocab)])
    return vocab

def sent_fet(sent, vocab):

    sent = sent.strip().lower().split()

    sent = [word for word in sent if word in vocab]

    return sent

def scan_corpus():
    vocab = load_vocab()

    corpus = []

    total = 0

    #writer = open('ap-stripped.txt', 'w')
    with open('ap.txt', 'r') as reader:
        for line in reader:
            stems = sent_fet(line, vocab)
            if len(stems) == 0:
                continue
            #writer.write(' '.join(stems) + '\n')
            #writer.write(line)
            corpus.append((stems, line.strip))
            total += len(stems)

    #print total
    return corpus

def gen_ex_docs():
    corpus = scan_corpus()

    top_doc_reader = open('top.doc')
    top_w_reader = open('top.w')

    with top_doc_reader, top_w_reader:
        for tid, (docs, words) in enumerate(zip(top_doc_reader, top_w_reader)):
            docs = docs.strip().split()
            words = set(words.strip().split())

            print '* Topic #', tid

            for doc in docs:
                stems, line = corpus[int(doc) - 1]
                stems.insert(0, ' ')
                stems.append(' ')

                output = "    * "
                for idx, stem in enumerate(stems):
                    if stem in words:
                        output += ' ... '
                        output += stems[idx - 1]
                        output += ' **'
                        output += stem
                        output += '** '
                        output += stems[idx + 1]

                        if len(output) > 80:
                            output += ' ...'
                            break
                print output

def main():
    #scan_corpus()
    gen_ex_docs()

    pass

if __name__ == "__main__":
    main()

# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
