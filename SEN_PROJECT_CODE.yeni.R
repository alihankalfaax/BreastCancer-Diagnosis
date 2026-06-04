
library(tidyverse)
library(factoextra)
library(corrplot)

# 1. Load the dataset
df <- read.csv("data.csv")

# 2. Data Cleaning
# Remove 'id' column as it is not needed for analysis

df_clean <- df %>%
  select(-id) %>%
  select(where(~ !all(is.na(.))))

# Check the structure of the cleaned data
str(df_clean)

# 3. Exploratory Data Analysis (EDA)

# Visualization 1: Diagnosis Distribution
# balance between Malignant (M) and Benign (B) samples
ggplot(df_clean, aes(x = diagnosis, fill = diagnosis)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Diagnosis Distribution",
       x = "Diagnosis",
       y = "Count") +
  scale_fill_manual(values = c("B" = "steelblue", "M" = "firebrick"))

# Visualization 2: Correlation Matrix
# To observe multicollinearity among features before PCA
numeric_data <- df_clean %>% select(-diagnosis)
cor_matrix <- cor(numeric_data)

# Plotting the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.6, tl.col = "black",
         title = "Correlation Matrix of Features", mar=c(0,0,1,0))

ggplot(df_clean, aes(x = diagnosis, fill = diagnosis)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Diagnosis Distribution",
       x = "Diagnosis",
       y = "Count") +
  scale_fill_manual(values = c("B" = "steelblue", "M" = "firebrick"))

# --- 4. PCA ANALYSE ---
#Here, we compress 30 different features and reduce them to 2 dimensions.


pca_result <- prcomp(numeric_data, scale = TRUE)

# --- GRAPH 3: PCA DECOMPOSITION GRAPH ---

# We will see how the red (cancer) and blue (clean) points are separated.


fviz_pca_ind(pca_result,
             geom.ind = "point",                # Only show dut not write
             col.ind = df_clean$diagnosis,      # Adjust the colors according to the diagnosis.
             palette = c("steelblue", "firebrick"), # Blue: Benign, Red: Malignant
             addEllipses = TRUE,                
             legend.title = "Diagnosis",
             title = "PCA: Malignant vs Benign Separation")

# --- 5. K-MEANS CLUSTERING ANALYSIS ---

# Setting seed ensures reproducibility of the results (getting same clusters every time)
set.seed(123)

# Applying K-Means algorithm
# We choose centers = 2 because we expect two groups: Benign and Malignant
# 'scale' is used to normalize features before clustering
km_res <- kmeans(scale(numeric_data), centers = 2, nstart = 25)

# --- VISUALIZATION: CLUSTER PLOT ---
# Visualizing the formed clusters in 2D space
fviz_cluster(km_res, data = numeric_data,
             palette = c("#2E9FDF", "#E7B800"), # Blue and Yellow palette
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "K-Means Clustering Results")

# --- VALIDATION: CONFUSION MATRIX ---
# Comparing the clusters found by the algorithm with the actual diagnosis labels
table(Real_Diagnosis = df_clean$diagnosis, Cluster_Found = km_res$cluster)



