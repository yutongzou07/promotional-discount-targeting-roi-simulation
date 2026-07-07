# promotional-discount-targeting-roi-simulation
# Promotional Discount Targeting: Customer Segmentation & ROI Simulation

**R analysis** — K-means customer segmentation combined with ROI/net-gain simulation to determine which customers should receive promotional discounts, and at what rate.

*Team project — coding by Yutong Zou and Sannidhi Patel.*

## Business Problem

Retailers often run blanket promotional discount campaigns that discount indiscriminately across the customer base, which is wasteful — full-price loyal customers don't need a discount to convert, while price-sensitive customers may respond strongly to one. This project segments customers by purchasing behavior, identifies which segment is most discount-responsive, and simulates the ROI of a **targeted** discount campaign against a **blanket** one to recommend an optimal discount rate.

## Approach

**1. Segmentation**
Cleaned and standardized customer transaction data (purchase amount, purchase frequency, subscription status, discount/promo usage, review ratings), then ran **K-means clustering (k=3)**, selected via elbow (WSS) and silhouette analysis. Resulting segments:
- **Cluster 1 — Price-sensitive shoppers**: high discount/promo usage, strongest uplift potential
- **Cluster 2 — Loyal full-price buyers**: low discount usage, high spend — discounting them is pure margin loss
- **Cluster 3 — Mixed/mid-value group**

**2. Cluster validation**
Rather than taking the segmentation at face value, validated it on four dimensions:
- **Silhouette score**: peaked at k=3 (0.222) — moderate but clearest separation among tested k values
- **WSS elbow**: confirmed k=3 as the point of diminishing returns
- **ANOVA**: significant differences across clusters (p < 0.001) on discount usage, promo usage, and purchase frequency — confirming the segments are behaviorally distinct, not arbitrary
- **Adjusted Rand Index (ARI)** across repeated runs: mean ARI of 0.960, indicating the clustering solution is highly stable and not an artifact of random initialization

**3. ROI simulation**
Since real campaign response data wasn't available, built a simulation-based business case instead of relying on historical lift data:
- Modeled a **diminishing-returns uplift function** (uplift rises with discount rate, then falls — reflecting realistic consumer response rather than a naive linear assumption)
- Compared **targeted** (Cluster 1 only) vs. **blanket** (all customers) discounting across a grid of discount rates (5–25%)
- Ran a **Monte Carlo simulation** (100 runs, ±5% noise on average customer spend) to test whether the optimal discount rate was sensitive to estimation error in the input data

## Key Findings

- Targeting Cluster 1 exclusively produced a substantially higher ROI than blanket discounting, since blanket campaigns waste discount spend on customers (Cluster 2) who would have purchased at full price anyway.
- The ROI-vs-discount-rate curve rose and then declined, consistent with real diminishing returns — validating that the model wasn't just rewarding "discount more" indefinitely. A supporting heatmap showed the highest-ROI region concentrated around a 10–15% discount rate, aligning with an external benchmark (McKinsey's cited industry baseline of ~10%).
- **ROI-maximizing discount rate: 5%.** **Net-gain-maximizing discount rate: 12%.** These diverge because a lower discount rate is more capital-efficient per dollar spent, while a slightly higher rate captures more total incremental revenue — a real strategic tradeoff between efficiency and scale that a business would need to choose between based on its goals.
- The Monte Carlo simulation showed the optimal discount rate was **stable under noise** (peak ROI 0.94 ± 0.00 across 100 runs with randomized spend), meaning the recommendation isn't fragile to small errors or fluctuations in customer spending data.

## Recommendation

Deploy a **targeted discount strategy** aimed specifically at Cluster 1 (price-sensitive shoppers), using a 5% or 12% discount rate depending on whether the business prioritizes ROI efficiency or total net revenue growth. Cluster 2 (full-price loyalists) should receive loyalty rewards instead of discounts, and Cluster 3 should receive lighter-touch re-engagement offers. Because the analysis is simulation-based rather than validated against real campaign response data, the recommended next step is a live A/B test comparing targeted vs. blanket discounting on a held-out customer segment before full rollout — along with ongoing monitoring for discount fatigue, margin erosion, and fair-treatment/privacy considerations across customer segments.

## Tools

R — tidyverse, cluster/factoextra (K-means, silhouette, elbow method), mclust (Adjusted Rand Index), Monte Carlo simulation, ggplot2

## Notes

This was a team project for a graduate data science course. Dataset is not included in this repository.
