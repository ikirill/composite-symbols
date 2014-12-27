
def f(x):
  x = None and False or not False
  x = x >> 1
  x = x << 2
  if x is not None: # no change
    if x is (not None):
      if x is None:
        return True
  andd = lambda x: x
  x = x or ord(x) and andd(x) # ord and andd should not change
  psi = phi = psi_phi_1 = 1
  print((psi, phi, psi_phi_1))

# Local Variables:
# eval: (flycheck-mode -1)
# End:
