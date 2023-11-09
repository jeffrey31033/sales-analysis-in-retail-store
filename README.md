# sales-analysis-in-retail-store

### Project Overview
---
The challenges that retail stores face are the complexity of consumer purchasing patterns, influenced by factors such as store size, promotional markdowns, and holiday period. The objective of this data analysis project is to provide insights into various aspects of the sales performance  on a period from 2010-02-05 to 2012-11-01. Through a comprehensive data analysis, my goal is to discern whether differences in sales outcomes exist given by holiday period, markdown approaches, and the different size and type of the stores and give some recommendations.

The project is categorized in three different phases. FIrsty, I use R conduct data analysis on different aspects on various factors. Second, I will give some recommendation based on the analysis I counducted. Finally, I would like to use Tableau to create visualizations that effectively convey the findings derived from the data.

### Data Sources
---
Retail Data: The primary dataset used for analysis was sourced from Kagle, including three excel sheet. The data contained historical sales data for 45 stores and several promotional markdown events.   [Data source](https://www.kaggle.com/datasets/manjeetsingh/retaildataset)


### Tools
---
- R - Conducting Data Analysis 
- Tableau - Visualizing Data

---
### What's this analysis discovered?
- The sales on holidays will be influenced by different types of stores
- The sales on holidays will be influenced by defferent sizes of stores
- The sales on holidays will be influenced by different department of products
- Different Type of stores have different top 10 sales on holidays


### First Phase: Conduct Exploratory Data Analysis  


At the first beginning, let's see the trends of total_sales given by different type of store. We can notice that the trends of type A and type B are quite similar, but the trend of type C is extremely different. 
<br>
<br>
<img width="494" alt="Trends on Total Sales Given by Different Type of Store" src="https://github.com/jeffrey31033/sales-analysis-in-retail-store/assets/149200070/700b0353-7bfb-4872-9bfd-f4e44e721541">


```r
sales_with_store_type %>%
    group_by(Size_Category, IsHoliday) %>%
    summarise(mean_sales = mean(Weekly_Sales, na.rm = TRUE)) %>%
    ggplot(aes(x = Size_Category, y = mean_sales, fill = IsHoliday)) + 
    geom_col(position = 'dodge') +
    labs(title = "Average Sales Differnence Betwen Holidays and Normal Days Given Store size")
```
<br>
Next, let's see whether any difference exists between sales on holidays and those on normal days given by different type of stores. You can discover that there is a difference between average sales on holidays and those on normal days in Type A and Type B stores. However, the average sales on holidays and normal days are quite similar in Type C store. 
<br>
<br>
<img width="494" alt="Average Sales Differnence Betwen Holidays and Normal Days Given Store size" src="https://github.com/jeffrey31033/sales-analysis-in-retail-store/assets/149200070/234c60db-f07e-491a-83e0-7a2bcdb4187d">

<br>

```r
sales_with_store_type %>%
    group_by(Type, IsHoliday) %>%
    summarize(mean_sales = mean(Weekly_Sales, na.rm = TRUE)) %>%
    ggplot(aes(x = Type, y = mean_sales, fill = IsHoliday)) +
    geom_col(position = "dodge") +
    labs(title = "Average Sales on Holidays and Normal Days Given by Different Type of Store",
         col = 'Holiday',
         y = 'avg_sales')
```
<br>
According to the pervious graphs, we can find that difference type of store may have impact on the average sales given by the date period. The discovery makes me curious about whether the other factors will influence the sales or not. Therefore, I decide to see whether the size of store will influence average sales or not. Firsty, I categorize the size of store into 3 categories, including LargeSize, MediumSize, and SmallSize. The result shows that the size of stores may have impact on the average sales. The difference in average sales between holidays and normal days will increase if the size of stores increase. 
<br>
<br>
<img width="467" alt="Average Sales Betwen Holidays and Normal Days Given Store size" src="https://github.com/jeffrey31033/sales-analysis-in-retail-store/assets/149200070/cb7b555f-d015-4ce1-aac3-d8e7b340b1c7">
<br>

```r
store_size_type <- sales_with_store_type %>%      
    select(Store, Size) %>%
    distinct(Store, Size) %>%
    mutate(group_number = ntile(desc(Size), n = 3))
for (i in 1:nrow(store_size_type)) {
    if (store_size_type$group_number[i] == 1) {
        store_size_type$Size_Category[i] = 'LargeSize'
    } else if (store_size_type$group_number[i] ==2) {
        store_size_type$Size_Category[i] = 'MediumSize'
    } else {
        store_size_type$Size_Category[i] = "SmallSize"
    }
}
sales_with_store_type <- sales_with_store_type %>%
    left_join(store_size_type, by = c("Store")) %>%
    select(-Size.y, -group_number) %>%
    rename(Size = Size.x)

sales_with_store_type %>%
    group_by(Size_Category, IsHoliday) %>%
    summarise(mean_sales = mean(Weekly_Sales, na.rm = TRUE)) %>%
    ggplot(aes(x = Size_Category, y = mean_sales, fill = IsHoliday)) + 
    geom_col(position = 'dodge') +
    labs(title = "Average Sales Betwen Holidays and Normal Days Given Store size")
```

Futhermore, I'm curious about whether department will influence the average sales on holidays and normal days. In order to get the information, I, firstly, calculate the difference between sales on holidays and sales on normal days and select the departments whose differnece on average sales between holidays and normal days is in Top 20. Secondly, I visualize the data and discover that departments will have impact on the average sales in different time period. 
<br>
<br>
<img width="465" alt="Difference between average sales between Holiday and Normal Day" src="https://github.com/jeffrey31033/sales-analysis-in-retail-store/assets/149200070/534885fa-48f3-48ac-a04e-cfe26e07a607">

<br>

```r
sales_with_store_type %>%
    group_by(Dept, IsHoliday) %>%
    summarize(mean_sales = mean(Weekly_Sales, na.rm = TRUE)) %>% 
    pivot_wider(names_from = IsHoliday, values_from = mean_sales, names_prefix = 'IsHoliday_') %>% 
    mutate(Difference_in_average_sales = IsHoliday_TRUE - IsHoliday_FALSE) %>% 
    arrange(desc(Difference_in_average_sales)) %>%
    head(20) %>%
    ggplot(aes(x = fct_rev(fct_reorder(Dept, Difference_in_average_sales)), y = Difference_in_average_sales)) +
    geom_col(fill ='coral') +
    labs(title = 'Difference Between Sales on Holidays and Normal days Given by Department',
         x = 'Top 20 Department',
         y = 'Difference between average sales between Holiday and Normal Day')
```

Next, I would like to find out the top 10 products with the largest difference in sales volume between holidays and non-holidays given by type of stores. The result shows that difference exist among different type of stores. 
<br>
<br>
<img width="690" alt="Top 10 Sales of Department Given by Different Type of Store" src="https://github.com/jeffrey31033/sales-analysis-in-retail-store/assets/149200070/2bb554f8-cb23-4810-97e3-921b3e1536b6">

```r
sales_with_store_type_dept_rank <- sales_with_store_type %>%
   group_by(Type, Dept) %>%
   summarize(mean_sales = round(mean(Weekly_Sales),2)) %>%
   arrange(Type, desc(mean_sales)) %>%
   mutate(rank_sales = rank(desc(mean_sales))) %>%
   filter(rank_sales %in% c(1:10))

sales_with_store_type_dept_rank %>%
    ggplot(aes(x = Dept, y = mean_sales, fill = Type, label = mean_sales)) +
    geom_col() +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    labs(title = "Top 10 Department of Products with the largest difference in sales between holidays and non-holidays")
```

<br> 
Top 10 products with the largest difference in sales volume between holidays and non-holidays in Type A Store
<br>
<br>
<img width="690" alt="Top 10 Department of Products in Type A Store" src="https://github.com/jeffrey31033/sales-analysis-in-retail-store/assets/149200070/9d9be695-cd09-4369-82db-a2c316f1d363">

<br>

```r
sales_with_store_type_dept_rank %>%
    filter(Type == 'A') %>%
    ggplot(aes(x = fct_rev(fct_reorder(Dept, mean_sales)), y = mean_sales)) + 
    geom_col(fill = "darkgoldenrod2") +
    labs(title = "Top 10 Department of Products with the largest difference in sales between holidays and non-holidays in Type A Store",
         x = "Dept",
         y= "Difference")
```

<br>
Top 10 products with the largest difference in sales volume between holidays and non-holidays in Type B 
<br>
<br>
<img width="692" alt="Top 10 Department of Products in Type B Store" src="https://github.com/jeffrey31033/sales-analysis-in-retail-store/assets/149200070/f1cc9f8c-405c-4039-a2d4-536b56122911">
<br>
<br>

```r
sales_with_store_type_dept_rank %>%
    filter(Type == 'B') %>%
    ggplot(aes(x = fct_rev(fct_reorder(Dept, mean_sales)), y = mean_sales)) + 
    geom_col(fill = "darkolivegreen4") +
    labs(title = "Top 10 Department of Products with the largest difference in sales between holidays and non-holidays in Type B Store",
         x = "Dept",
         y = "Difference")
```

Top 10 products with the largest difference in sales volume between holidays and non-holidays in Type C
<br>
<br>
<img width="688" alt="Top 10 Department of Products in Type C Store" src="https://github.com/jeffrey31033/sales-analysis-in-retail-store/assets/149200070/c5cc8d2f-f97a-4be4-bf71-ecfcf34e556a">
<br>

```r
sales_with_store_type_dept_rank %>%
    filter(Type == 'C') %>%
    ggplot(aes(x = fct_rev(fct_reorder(Dept, mean_sales)), y = mean_sales)) + 
    geom_col(fill = "cyan4") +
    labs(title = "Top 10 Department of Products with the largest difference in sales between holidays and non-holidays in Type C Store",
         x = "Dept",
         y = "Difference")
```

<br>

### Recommendation






