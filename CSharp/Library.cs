using System;

namespace ABP.Bench
{

public interface IBench {
    object Run(int n);
}

namespace CSharp
{
    public class Bench0001: IBench {
        public object Run(int n) {
            var ret = Run(n, n / 2);
            return ret;
        }

        int Run(int n, int k) {
            if (k == 0 || k == n)
                return 1;
            return Run(n-1, k-1) + Run(n-1, k);
        }
    }
}  // namespace CSharp

}  // namespace ABP.Bench
