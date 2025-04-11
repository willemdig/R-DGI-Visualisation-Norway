import plotly.express as px
import pandas as pd

df = pd.read_csv("python/Digital_by_Design_Weights.csv")  # or manually construct the DataFrame


fig = px.sunburst(
    df,
    path=["Dimension", "Facet", "Question"],
    values="Weight_CS",
    title="OECD DGI 2023 – Dimension 1: Digital by Design",
)
fig.show()

fig.write_html("html/digital_by_design.html")


