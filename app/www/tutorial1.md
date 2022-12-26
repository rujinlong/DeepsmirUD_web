Novel regulatory effect
================

Through the “Novel regulatory effect” tab, users are directed to access
the information of regulatory effects of small molecule-miRNA pairs
curated from Psmir and Verse.

## 1. Curated small molecule-miRNA pairs from Psmir

### Step 1. Database selection

It provides a series of databases, which are made by using different
p-values to filter data of the Psmir database. The databases are
0.05_op, 0.05_non_op, appv_0.01_op_up, appv_0.01_op_down,
appv_0.01_non_op_up, appv_0.01_non_op_down, unappv_0.01_op_up,
unappv_0.01_op_down, unappv_0.01_non_op_up, and unappv_0.01_non_op_down.
appv is for FDA-approved. unappv is for FDA-unapproved. op is for
overlapped, representing a miRNA or a small molecule in these small
molecule-miRNA pairs are overlapped with miRNAs or small molecules in
the DeepsmirUD training dataset. The loading page of the Psmir databases
is shown in **Figure 1**. After the ‘0.05_non_op’ database is selected,
tables and plots are loaded as in **Figures 2-4**.

<center>

<figure>
<img src="nrFigure1.png" style="width:95.0%"
alt="Figure 1. Databases of predicted regulatory effects of the Psmir pairs" />
<figcaption aria-hidden="true">Figure 1. Databases of predicted
regulatory effects of the Psmir pairs</figcaption>
</figure>

</center>

</br>

<center>

<figure>
<img src="nrFigure2.png" style="width:95.0%"
alt="Figure 2. Novel regulatory effect data" />
<figcaption aria-hidden="true">Figure 2. Novel regulatory effect
data</figcaption>
</figure>

</center>

**Figure 2** shows the tables and plots after page loading. The tables
provide small molecule-miRNA pair information, their predicted
probabilities of regulatory effects and labels of down- or
up-regulation. For plots, we provide the matrix of correlation between
models, histogram, distribution, and heatmap (**Figure 3** and **Figure
4**). The plots interactively change with data in the tables.

<center>

<figure>
<img src="nrFigure3.png" style="width:95.0%"
alt="Figure 3. Correlation matrix, histogram, and probability distribution plots" />
<figcaption aria-hidden="true">Figure 3. Correlation matrix, histogram,
and probability distribution plots</figcaption>
</figure>

</center>

</br>

<center>

<figure>
<img src="nrFigure4.png" style="width:95.0%"
alt="Figure 4. Heatmap plot" />
<figcaption aria-hidden="true">Figure 4. Heatmap plot</figcaption>
</figure>

</center>

</br>

### Step 2. Applying filters

#### (1). by small molecules

In a certain database, all small molecules are listed at dropdown
‘Filter by small molecule (compound)’ (**Figure 5**). If you do not know
which small molecules should be of your interest, you can follow what is
shown in **Figure 5** for filtering small molecule-miRNA pairs. If you
have an idea of which small molecule should be used, you can filter
small molecule-miRNA pairs according to **Figure 6**.

<center>

<figure>
<img src="nrFigure5.png" style="width:95.0%"
alt="Figure 5. Select small molecule-miRNA pairs by applying a small molecule filter ‘maprotiline’ and ‘sanguinarine’" />
<figcaption aria-hidden="true">Figure 5. Select small molecule-miRNA
pairs by applying a small molecule filter ‘maprotiline’ and
‘sanguinarine’</figcaption>
</figure>

</center>

</br>

<center>

<figure>
<img src="nrFigure6.png" style="width:95.0%"
alt="Figure 6. Select small molecule-miRNA pairs by typing in a small molecule ‘vorinostat’." />
<figcaption aria-hidden="true">Figure 6. Select small molecule-miRNA
pairs by typing in a small molecule ‘vorinostat’.</figcaption>
</figure>

</center>

</br>

If we drag the table scroll bar rightmost, it shows that miR-433 is
upregulated by vorinostat and miR-448 is downregulated by vorinostat
(**Figure 7**).

<center>

<figure>
<img src="nrFigure7.png" style="width:95.0%"
alt="Figure 7. Regulation types of vorinostat-miRNA pairs." />
<figcaption aria-hidden="true">Figure 7. Regulation types of
vorinostat-miRNA pairs.</figcaption>
</figure>

</center>

</br>

#### (2). by miRNAs

Similarly, we can select one of the miRNAs in the selected database to
filter small molecule-miRNA pairs. There are 206 pairs left after a
miR-380 filter is applied (**Figure 8**). Similar to filtering pairs by
small molecules, if you have an idea of which miRNA should be used in
your case, you can type in a miRNA, for example, miR-433 (**Figure 9**).

<center>

<figure>
<img src="nrFigure8.png" style="width:95.0%"
alt="Figure 8. Select small molecule-miRNA pairs by applying a small molecule filter ‘miR-380’." />
<figcaption aria-hidden="true">Figure 8. Select small molecule-miRNA
pairs by applying a small molecule filter ‘miR-380’.</figcaption>
</figure>

</center>

</br>

<center>

<figure>
<img src="nrFigure9.png" style="width:95.0%"
alt="Figure 9. Select small molecule-miRNA pairs by typing in a miRNA ‘miR-433’." />
<figcaption aria-hidden="true">Figure 9. Select small molecule-miRNA
pairs by typing in a miRNA ‘miR-433’.</figcaption>
</figure>

</center>

</br>

<center>

<figure>
<img src="nrFigure10.png" style="width:95.0%"
alt="Figure 10. Figure 10. Regulation types of small molecule-miR-7 pairs." />
<figcaption aria-hidden="true">Figure 10. Figure 10. Regulation types of
small molecule-miR-7 pairs.</figcaption>
</figure>

</center>

</br>

If we drag the table scroll bar rightmost, we can see the predicted
down-regulation and up-regulation types of small molecule-miR-448 pairs
(**Figure 10**).

#### (3). by multiple conditions

Deepsmir-Web also allows users to screen the regulation pairs with
multiple conditions, for example, miR-433, FDA-approved, and P-value
(**Figure 11**).

<center>

<figure>
<img src="nrFigure11.png" style="width:95.0%"
alt="Figure 11. Small molecule-miRNA pairs filtered by multiple conditions." />
<figcaption aria-hidden="true">Figure 11. Small molecule-miRNA pairs
filtered by multiple conditions.</figcaption>
</figure>

</center>

</br>

## 2. Curated small molecule-miRNA pairs from Verse

Similarly, for getting the regulatory effects of the Verse pairs users
can apply as the same procedures as in Psmir (**Figure 12**).

<center>

<figure>
<img src="nrFigure12.png" style="width:95.0%"
alt="Figure 12. Regulatory effects of small molecule pairs curated from Verse." />
<figcaption aria-hidden="true">Figure 12. Regulatory effects of small
molecule pairs curated from Verse.</figcaption>
</figure>

</center>

</br>
