def read_st_file(filepath):
  with open(filepath) as f:
    lines = f.readlines()
    state_ids = lines[0][2:-1]
    tuples = []
    for l in lines[1:]:
      words = l.split()
      r = int(words[0])
      c = int(words[1])
      val = float(words[2])
      tuples.append((r, c, val))
    return (state_ids, tuples)
