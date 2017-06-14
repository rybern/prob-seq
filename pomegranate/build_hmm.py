from pomegranate import *
from read_st import read_st_file

def build_hmm(trans_file, state_map):
  (state_aliases, values) = read_st_file(trans_file)

  model = HiddenMarkovModel()

  model.add_states(*state_map.values())

  for (r, c, val) in values:
    source = None
    if r == 0:
      source = model.start
    elif r-1 < len(state_aliases):
      source = state_map[state_aliases[r - 1]]

    dest = None
    if c == len(state_aliases):
      dest = model.end
    elif c < len(state_aliases):
      dest = state_map[state_aliases[c]]

    model.add_transition(source, dest, val)

  model.bake()   

  return model

def build_hmm_default(trans_file):
  ks = ['A', 'C', 'B', 'E', 'D', 'I', 'O', 'N', 'P', 'S', 'R', 'T', '~']
  dist = Distribution(NormalDistribution(1,1))
  # dist = DiscreteDistribution({k: 1.0 / len(ks) for k in ks})
  state_map = {k: State(dist, name=k) for k in ks}
  return build_hmm(trans_file, state_map)

build_hmm_default("test.st")
