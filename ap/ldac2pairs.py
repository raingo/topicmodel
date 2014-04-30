#!/usr/bin/env python

"""
Python source code - replace this with a description of the code and write the code below this text.
"""
def gen_pairs(filename):

    with open(filename) as reader:
        for doc_id, line in enumerate(reader):
            fields = line.strip().split()
            for f in fields[1:]:
                wcnt = f.split(':')
                for i in range(int(wcnt[1])):
                    yield doc_id, wcnt[0], 1

def main():

    with open('ap-pairs.dat', 'w') as writer:
        for (doc_id, word_id, cnt) in gen_pairs('ap.dat'):
            writer.write('%s %s %s\n' % (doc_id, word_id, cnt));

    pass

if __name__ == "__main__":
    main()

# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
