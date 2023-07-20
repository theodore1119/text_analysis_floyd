# -*- coding: utf-8 -*-
"""topic model.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1PpTp6Iz3eBkMIUq5jD7gouFVb5peUyxe
"""

! pip install bertopic
from bertopic import BERTopic
from umap import UMAP
import pandas as pd
import numpy as np

all_text_clean = pd.read_excel("all_floyd_cleaned_final.xlsx")

# Initiate UMAP
umap_model = UMAP(n_neighbors=15,
                  n_components=5,
                  min_dist=0.0,
                  metric='cosine',
                  random_state=388)
# Initiate BERTopic
topic_model = BERTopic(umap_model=umap_model, language="english", calculate_probabilities=True)
# Run BERTopic model
topics, probabilities = topic_model.fit_transform(all_text_clean['text_clean'].astype('str'))

# Get the list of topics
topic_model.get_topic_info()

# Visualize top topic keywords
topic_model.visualize_barchart(top_n_topics=10)

# Visualize connections between topics using hierachical clustering
topic_model.visualize_hierarchy(top_n_topics=10)

# Get the topic predictions
topic_prediction = topic_model.topics_[:]
# Save the predictions in the dataframe
all_text_clean['topic_prediction'] = topic_prediction
# Take a look at the data
all_text_clean.head()

# Subset the top 6 topics
text_top_topics = all_text_clean.loc[all_text_clean['topic_prediction'].isin([0,1,2,3,4,5])]
# Frequency for each topic
top_topic_freq = text_top_topics['topic_prediction'].value_counts()
topic_freq = top_topic_freq.reset_index()
topic_freq.columns = ['Topic', 'Frequency']
topic_freq

import seaborn as sns
# plot with seaborn barplot
sns.set(font_scale=1)
sns.set_style("white")
sns.barplot(data=topic_freq, x='Topic', y='Frequency', color='steelblue')

text_top_topics.groupby(['topic_prediction','party'])['topic_prediction'].count()

import matplotlib.pyplot as plt
topic_count = pd.read_excel("topic_count.xlsx")
sns.set_palette(sns.color_palette(["#4374B3","#A62C2B"]))
ax = sns.barplot(x="Topic", y="Frequency", hue="Party", data=topic_count, ci=None)
for p,value in zip(ax.patches, topic_count['prop_party']):
    ax.annotate("{:.2%}".format(value), xy=(p.get_x()+p.get_width()/2, p.get_height()),
                ha='center', va='bottom', fontsize=8)
plt.legend(title="")