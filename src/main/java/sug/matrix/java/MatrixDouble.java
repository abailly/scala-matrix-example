package sug.matrix.java;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class MatrixDouble {

    private static final int CUTOFF = 128;

    protected final double[] matrix;

    private int              line;

    private int              col;

    public MatrixDouble(int ns) {
        this(ns, ns);
    }

    /**
     * @param matrix
     */
    public MatrixDouble(MatrixDouble matrix) {
        this(matrix.line);
        for (int i = 0; i < line; i++)
            for (int j = 0; j < col; j++)
                this.matrix[i * line + j] = matrix.matrix[i * line + j];

    }

    /**
     * @param l
     * @param c
     */
    public MatrixDouble(int l, int c) {
        this.line = l;
        this.col = c;
        matrix = new double[l * c];
    }

    public MatrixDouble(double[][] ds) {
        matrix = new double[ds.length * ds[0].length];
        this.line = ds.length;
	this.col = ds[0].length;
	for (int i = 0; i < line; i++)
	  for (int j = 0; j < col; j++)
	    this.matrix[i * line + j] = ds[i][j];
    }
  
    public MatrixDouble(double[] ds) {
      matrix = ds;
      this.line = (int) Math.sqrt(ds.length);
      this.col = this.line;
    }
  

    /**
     * Returns the n <sup>th </sup> power of this matrix.
     * 
     * @param n
     *            the power. Must be positive or null.
     * @param res
     *            matrix where the result should be stored. Must be same size as
     *            this matrix with all elements initialized with null.
     * @return the result Matrix object with transition matrix equals the n
     *         <sup>th </sup> power of this matrix's transition.
     */
    public MatrixDouble power(int n, MatrixDouble res) {
        int l = line;
        if (line != col) throw new IllegalStateException("Cannot compute power of a non square matrix");
        double[] tmp = new double[l * l];
        Arrays.fill(tmp, 0);
        for (int k = 0; k < n - 1; k++) {
            for (int i = 0; i < l; i++) {
                for (int j = 0; j < l; j++) {
                    double cell = 0;
                    for (int m = 0; m < l; m++) {
                        if (k == 0)
                            cell += (matrix[i * line + m] * matrix[m * line + j]);
                        else
                            cell += (res.matrix[i * line + m] * matrix[m * line + j]);
                    }
                    tmp[i * l + j] = cell;
                }
            }
            /* copy to res */
            System.arraycopy(tmp, 0, res.matrix, 0, l * l);
        }
        return res;
    }

    public int getLine() {
        return line;
    }

    public String toString() {
        final String ln = System.getProperty("line.separator");
        StringBuffer sb = new StringBuffer("{");
        for (int i = 0; i < line; i++) {
            sb.append("{");
            for (int j = 0; j < col; j++) {
                String s = Double.toString(matrix[i * line + j]);
                sb.append(s).append(',');
            }
            if (sb.length() > 1) sb.deleteCharAt(sb.length() - 1);
            sb.append("}").append(ln);
        }

        return sb.append("}").toString();
    }

  public double get(int i, int j) {
    return matrix[i * line + j];
  }
  
  /*
     * (non-Javadoc)
     * 
     * @see rationals.algebra.SemiRing#plus(rationals.algebra.SemiRing)
     */
    public MatrixDouble plus(MatrixDouble o) {
        if (o == null) throw new IllegalArgumentException("Null argument");
        if (col != o.col || line != o.line)
            throw new IllegalArgumentException("Incompatible matrices dimensions : cannot add non square matrices");
        int l = line;
        int c = col;
        MatrixDouble res = MatrixDouble.zero(l, c);
        for (int i = 0; i < l; i++)
            for (int j = 0; j < c; j++)
                res.matrix[i * line + j] = matrix[i * line + j] + o.matrix[i * line + j];
        return res;
    }

    /*
     * (non-Javadoc)
     * 
     * @see rationals.algebra.SemiRing#mult(rationals.algebra.SemiRing)
     */
    public MatrixDouble times(MatrixDouble o) {
        if (o == null) throw new IllegalArgumentException("Null argument");
        if (col != o.line) throw new IllegalArgumentException("Incompatible matrices dimensions");
        int l = line; // lines
        int c = o.col; // cols
        int m = col;
        MatrixDouble res = MatrixDouble.zero(l, c);
        for (int i = 0; i < l; i++) {
            for (int j = 0; j < c; j++) {
                double cell = 0;
                for (int k = 0; k < m; k++) {
                    cell += (matrix[i * line + k] * o.matrix[k * line + j]);
                }
                res.matrix[i * line + j] = cell;
            }
        }
        return res;
    }

    /*
     * (non-Javadoc)
     * 
     * @see rationals.algebra.SemiRing#one()
     */
    public MatrixDouble one() {
        if (line != col) throw new IllegalStateException("Cannot get unit matrix on non-square matrices");
        return one(line);
    }

    /*
     * (non-Javadoc)
     * 
     * @see rationals.algebra.SemiRing#zero()
     */
    public MatrixDouble zero() {
        return zero(line, col);
    }

    public int getCol() {
        return col;
    }

    /**
     * Factory method for creating Matrix instances with coefficients in a
     * certain SemiRing.
     * 
     * @param sr
     *            a SemiRing instance. Used to get one and zero.
     * @return a new zero matrix.
     */
    public static MatrixDouble zero(int line, int col) {
        MatrixDouble m = new MatrixDouble(line, col);
        for (int i = 0; i < line; i++)
            for (int j = 0; j < col; j++)
                m.matrix[i * line + j] = 0;
        return m;
    }

    /**
     * Factory method for creating unit Matrix instances with coefficients in a
     * certain SemiRing.
     * 
     * @param sr
     *            a SemiRing instance. Used to get one and zero.
     * @return a new unit square matrix.
     */
    public static MatrixDouble one(int dim) {
        MatrixDouble m = new MatrixDouble(dim);
        for (int i = 0; i < dim; i++)
            for (int j = 0; j < dim; j++)
                m.matrix[i * dim + j] = (i == j) ? 1 : 0;
        return m;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + col;
        result = prime * result + line;
        result = prime * result + Arrays.hashCode(matrix);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        MatrixDouble other = (MatrixDouble) obj;
        if (col != other.col) return false;
        if (line != other.line) return false;
        for (int i = 0; i < line; i++)
            for (int j = 0; j < col; j++)
                if (matrix[i * line + j] != other.matrix[i * line + j]) return false;
        return true;
    }

    /**
     * Compute matrix multiplication in parallel.
     * 
     * @param matrix2
     *            to keep things simple, dimensions must be powers of 2.
     * @return
     */
    public MatrixDouble ptimes(final MatrixDouble matrix2) {
        if (matrix2.line < CUTOFF) return times(matrix2);
        int blocks = matrix2.line / CUTOFF;
        // explode stuff
        final MatrixDouble[][] As = divideInBlocks(blocks);
        final MatrixDouble[][] Bs = matrix2.divideInBlocks(blocks);
        final MatrixDouble[][] Cs = new MatrixDouble[blocks][blocks];
        for (int i = 0; i < blocks; i++)
            for (int j = 0; j < blocks; j++)
                Cs[i][j] = zero(matrix2.line / blocks, matrix2.line / blocks);
        List<Runnable> res = new ArrayList<Runnable>();
        for (int i = 0; i < blocks; i++)
            for (int j = 0; j < blocks; j++) {
                for (int k = 0; k < blocks; k++) {
                    final int ip = i;
                    final int jp = j;
                    final int kp = k;
                    Runnable mult1 = new Runnable() {

                        @Override
                        public void run() {
                            synchronized (Cs[ip][jp]) {
                                MatrixDouble times = As[ip][kp].times(Bs[kp][jp]);
                                MatrixDouble localres = Cs[ip][jp];
                                if (localres == null)
                                    Cs[ip][jp] = times;
                                else
                                    Cs[ip][jp] = Cs[ip][jp].plus(times);
                            }
                        }
                    };
                    res.add(mult1);
                }
            }
        // compute
        ExecutorService executor = Executors.newFixedThreadPool(4);
        for (Runnable run : res)
            executor.execute(run);
        // merge back
        try {
            executor.shutdown();
            if (!executor.awaitTermination(100, TimeUnit.SECONDS))
                throw new IllegalStateException("Multiplication took too long");
            else
                return merge(Cs);
        } catch (InterruptedException e) {
            e.printStackTrace();
            throw new IllegalStateException("Thread interrupted");
        }
    }

    private int bit(int highestOneBit) {
        int ret = 0;
        while (highestOneBit != 1) {
            highestOneBit >>= 1;
            ret++;
        }
        return ret;
    }

    public static MatrixDouble merge(MatrixDouble[][] cs) {
        int blocks = cs.length;
        int blockSize = cs[0][0].col;
        int ln = blocks * blockSize;
        MatrixDouble res = new MatrixDouble(ln);
        for (int i = 0; i < ln; i++) {
            for (int j = 0; j < ln; j++) {
                int ib = i / blockSize;
                int jb = j / blockSize;
                int ri = i % blockSize;
                int rj = j % blockSize;
                res.matrix[i * ln + j] = cs[ib][jb].matrix[ri * blockSize + rj];
            }
        }
        return res;
    }

    public MatrixDouble[][] divideInBlocks(int numberOfBlocksPerDimension) {
        MatrixDouble[][] res = new MatrixDouble[numberOfBlocksPerDimension][numberOfBlocksPerDimension];
        int ln = line / numberOfBlocksPerDimension;
        for (int i = 0; i < numberOfBlocksPerDimension; i++) {
            for (int l = 0; l < numberOfBlocksPerDimension; l++) {
                MatrixDouble block = new MatrixDouble(ln);
                for (int j = 0; j < ln; j++)
                    for (int k = 0; k < ln; k++)
                        block.matrix[j * ln + k] = matrix[(i * ln + j) * line + (l * ln + k)];
                res[i][l] = block;
            }
        }
        return res;
    }
}
