def validator(str):

  def converter(str):
    dict = {
      '(': 1,
      ')': 2,
      '[': 3,
      ']': 4,
      '{': 5,
      '}': 6
    }

    return [dict.get(x, 0) for x in str]

  def lower(list):
    m = 0
    f = False

    for k, v in enumerate(list):
      if v != 0:
        if list[m] == 0:
          m = k
        elif list[m] + 1 == v:
          list[k] = 0
          list[m] = 0

          f = True

    if f:
      lower(list)

  def indexNot(list, notVal):
    r = -1

    for k, v in enumerate(list):
      if v != notVal:
        r = k
        break

    return r

  seq = converter(str)
  lower(seq)

  return indexNot(seq, 0)

assert validator('{}[]()')   == -1, "test isn't passed"
assert validator('{[(]}')    ==  2, "test isn't passed"
assert validator('[{}{}]()') == -1, "test isn't passed"
assert validator('[{}{}](}') ==  6, "test isn't passed"
assert validator('{[(')      ==  0, "test isn't passed"
assert validator('(}{)')     ==  1, "test isn't passed"

print("if it's clean above, all tests are passed")
