def mandel(a2: u24, a4: u24) -> u24:
  def rec6(a8: Bool, a10: Bool, a12: Bool) -> u24:
  if ((a8 >= 64) or (((a10 * a10) + (a12 * a12)) >= 4)):
    return a8
  else:
    return rec6((a8 + 1))((((a10 * a10) - (a12 * a12)) + a2))((((2 * a10) * a12) + a4))
  return rec6(0)(0)(0)


def main():
  return 0

