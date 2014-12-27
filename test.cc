#include "../somefile.h"

int main(const Type&& x, // no change
  const Type && x) // change
{
  int x = NULL + 1 // no change with ignore-indentation set to nil
               + 2;
  int y = NULL + 1
              + 2;
  x &&= 1;
  x ||= 1;
  while (x --> 0); // no change
  if (!x && x != 1 || x == 1);
  x >>= 1;
  x <<= 1;
  cout << x << y;
  cin >> x >> y;
  return 0;

  alpha = 1;
  alpha_beta = 1;
  phi = psi = Omega = omega = omega_1; // all should change
  alphabeta = 1; // no change
}

// Local Variables:
// eval: (flycheck-mode -1)
// End:
