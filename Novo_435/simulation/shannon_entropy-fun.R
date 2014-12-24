shannon.entropy <- function(p)
{
        if (min(p) < 0 || sum(p) <= 0)
                return(NA)
        p.norm <- p[p>0]/sum(p)
        -sum(log2(p.norm)*p.norm)
}


shannon.entropy(population$wt)
