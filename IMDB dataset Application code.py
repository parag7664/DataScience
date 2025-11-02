#!/usr/bin/env python
# coding: utf-8

# # SCIENTIFIC PROGRAMMING IN PYTHON
# # IMDB DATASET
# # PARAG SONI - R00208553

# In[1]:


import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as pl
from scipy.stats import pearsonr


# In[2]:


movie_data = pd.read_csv("movie_metadata.csv")


# In[3]:


print(movie_data.shape)


# In[ ]:


while True:

    try:
        (print("\nPlease select one of the following options:\n-------------------------------------------\n\n\t"
               "1. Most successful directors or actors \n\t"
               "2. Film comparison \n\t"
               "3. Analyse the distribution of gross earnings  \n\t"
               "4. Genre Analysis \n\t"
               "5. Earnings and IMDB scores \n\t"
               "6. Exit"))
        choice = int(input("option: "))
        while choice in range(0, 7):
            option = choice
            if option == 1:
                # MENU OPTION 1 - MOST SUCCESSFUL DIRECTORS OR ACTORS
                print(("\n1. Top Directors\n2. Top Actors \n"))
                selection = int(input("option: "))

                if selection == 1:
                    # ----- DROP NULL VALUES IN COLUMN "director_name" ----- #
                    movie_data.dropna(subset=["director_name"], inplace=True)

                    # ----- FILL NULL VALUES WITH "0" IN COLUMN "gross" ----- #
                    movie_data.gross.fillna(float(0), inplace=True)

                    # New DataFrame of "director_name" and "gross" columns.
                    director_gross_df = movie_data[["director_name", "gross"]]

                    # Sort that DataFrame according to "gross" values. Reset indexes.
                    sorted_director_gross_df = director_gross_df.sort_values(["gross"], ascending=False).reset_index(
                        drop=True)

                    top_directors = []  # Top Directors list
                    gross = []  # Gross film earnings list

                    number_of_directors = int(input("Please Enter The number of directors : "))

                    length = len(sorted_director_gross_df)  # Number of rows in the DataFrame
                    j = 0  # to control the number of directors
                    for i in range(length):
                        if sorted_director_gross_df["director_name"][i] not in top_directors:
                            top_directors.append(sorted_director_gross_df["director_name"][i])
                            gross.append(sorted_director_gross_df["gross"][i])
                            j += 1
                            if j == number_of_directors:
                                break

                    print("top_directors", top_directors)
                    d = {'top_directors':top_directors, 'gross':gross}
                    df = pd.DataFrame(d)
                    sort_df = df.sort_values(["gross"],ascending=True)
                    ax = sort_df.plot.barh(x='top_directors', y='gross')
                    pl.show()
                    break
                if selection == 2:
                    movie_data.dropna(subset=["actor_1_name"], inplace=True)
                    print()
                    number_of_actors = int(input("\n Please enter the number of actors: "))

                    # Take "actor_1_name" and "gross" columns and create new DataFrame.
                    actor_gross_df = movie_data[["actor_1_name", "gross"]]

                    # Sort that DataFrame according to "gross" values. Reset indexes.
                    sorted_actor_gross_df = actor_gross_df.sort_values(["gross"], ascending=False).reset_index(drop=True)

                    top_actors = []  # Top Actors list
                    gross = []  # Gross film earnings list

                    length = len(sorted_actor_gross_df)  # Number of rows in the DataFrame
                    j = 0  # to control the number of actors
                    for i in range(length):
                        if sorted_actor_gross_df["actor_1_name"][i] not in top_actors:
                            top_actors.append(sorted_actor_gross_df["actor_1_name"][i])
                            gross.append(sorted_actor_gross_df["gross"][i])
                            j += 1
                            if j == number_of_actors:
                                break
                    actors = {'top_actors': top_actors, 'gross': gross}
                    actors_df = pd.DataFrame(actors)
                    actors_sort_df = actors_df.sort_values(["gross"], ascending=True)
                    ax = actors_sort_df.plot.barh(x='top_actors', y='gross')
                    pl.show()
                    break
            elif option == 2:

                # MENU OPTION 2 - FILM COMPARISON
                movie_data.dropna(subset=["gross"], inplace=True)
                while True:
                    while True:
                        print("\nPlease enter the name of first film: ")
                        movie_1 = input("(1): ")
                        if movie_data.movie_title.isin([movie_1 + "\xa0"]).any() == True:
                            break
                        else:
                            print("\nThe movie name you entered is not present in the dataset. Please try again.")
                    while True:
                        print("\nPlease enter the name of second film: ")
                        movie_2 = input("(2): ")
                        if movie_data.movie_title.isin([movie_2 + "\xa0"]).any() == True:
                            break
                        else:
                            ("\nThe movie name which you entered is not present in the dataset. Please try again.")
                    if movie_1 != movie_2:
                        break
                    else:
                        print("\nPlease check the movie names. You have entered same name for both movies.")

                while True:
                    print(
                        "\nSelect the parameter to compare by:\n--------------------------------\n\n\t"
                        "i. IMDB Scores\n\t"
                        "ii. Gross Earning\n\t"
                        "iii. Movie Facebook Like")
                    selection = int(input("Compare parameter: "))

                    if selection == 1:
                        # Getting imdb_score values
                        imdb_score_1 = movie_data["imdb_score"][
                            movie_data[movie_data['movie_title'] == movie_1 + '\xa0'].index.values.astype(int)[0]]
                        imdb_score_2 = movie_data["imdb_score"][
                            movie_data[movie_data['movie_title'] == movie_2 + '\xa0'].index.values.astype(int)[0]]

                        # PLOTTING A BAR GRAPH
                        movie_name = [movie_1,movie_2]
                        imdb_score = [imdb_score_1, imdb_score_2]
                        movie_by_imdb_score = {'movie_name': movie_name, 'imdb_score': imdb_score}
                        movie_by_imdb_score_df = pd.DataFrame(movie_by_imdb_score)
                        ax = movie_by_imdb_score_df.plot.bar(x='movie_name', y='imdb_score')
                        pl.title("Comparison of " + movie_1 + " and " + movie_2 + " by IMDB scores")
                        pl.show()
                        break
                    elif selection == 2:
                        # Getting gross earning values
                        gross_1 = movie_data["gross"][movie_data[movie_data['movie_title'] == movie_1 + '\xa0'].index.values.astype(int)[0]]
                        gross_2 = movie_data["gross"][movie_data[movie_data['movie_title'] == movie_2 + '\xa0'].index.values.astype(int)[0]]

                        # PLOTTING A BAR GRAPH
                        movie_name = [movie_1, movie_2]
                        gross = [gross_1, gross_2]
                        movie_by_gross = {'movie_name': movie_name, 'gross': gross}
                        movie_by_gross_df = pd.DataFrame(movie_by_gross)
                        ax = movie_by_gross_df.plot.bar(x='movie_name', y='gross')
                        pl.title("Comparison of " + movie_1 + " and " + movie_2 + " by gross income")
                        pl.show()
                        break
                    elif selection == 3:
                        # Getting movie facebook likes values
                        fb_like_1 = movie_data["movie_facebook_likes"][
                            movie_data[movie_data['movie_title'] == movie_1 + '\xa0'].index.values.astype(int)[0]]
                        fb_like_2 = movie_data["movie_facebook_likes"][
                            movie_data[movie_data['movie_title'] == movie_2 + '\xa0'].index.values.astype(int)[0]]

                        # PLOTTING A BAR GRAPH
                        movie_name = [movie_1, movie_2]
                        fb_like = [fb_like_1, fb_like_2]
                        movie_by_fb_like = {'movie_name': movie_name, 'fb_like': fb_like}
                        movie_by_fb_like_df = pd.DataFrame(movie_by_fb_like)
                        ax = movie_by_fb_like_df.plot.bar(x='movie_name', y='fb_like')
                        pl.title("Comparison of " + movie_1 + " and " + movie_2 + " by Facebook likes scores")
                        pl.show()
                        break
                    else:
                        print("Invalid selection! Please try again.")

            elif option == 3:
                # MENU OPTION 3 - ANALYSE THE DISTRIBUTION OF GROSS EARNINGS
                # DROPPING ROWS WITH NULL VALUES IN GROSS
                movie_data.dropna(subset=["gross"], inplace=True)

                max_year = int(movie_data.title_year.max())
                min_year = int(movie_data.title_year.min())

                print("\nPlease enter the start year: ")
                start_year = int(input("Start year"))
                print("\nSelect the end year: ")
                end_year = int(input("End year"))
                if ((start_year >= min_year) and (start_year <= max_year) and (end_year <= max_year) and (end_year >= min_year)):
                    # Creating new DataFrame.
                    year_gross_df = movie_data[["title_year", "gross"]]

                    min_gross = []
                    max_gross = []
                    avg_gross = []
                    year = []
                    for i in range(start_year, end_year + 1):
                        boolean_year_exist = (year_gross_df["title_year"] == i)
                        total_gross_of_year = year_gross_df.loc[boolean_year_exist]
                        min_gross.append(total_gross_of_year.gross.min())
                        max_gross.append(total_gross_of_year.gross.max())
                        avg_gross.append(total_gross_of_year.gross.mean())
                        year.append(i)

                    # PLOTTING A LINE GRAPH
                    df = pd.DataFrame({
                        'min_gross': min_gross,
                        'max_gross': max_gross,
                        'avg_gross': avg_gross
                    }, index=year)
                    lines = df.plot.line()
                    pl.title("THE DISTRIBUTION OF GROSS EARNINGS" + "(" + str(start_year) + "-" + str(end_year) + ")")
                    pl.show()
                    break
                else:
                    print("Invalid input! Please enter films that made between 1920 and 2016.")

            elif option == 4:
                # MENU OPTION 4 - GENRE ANALYSIS
                movie_data.dropna(subset=["movie_title"], inplace=True)
                movie_data.dropna(subset=["genres"], inplace=True)
                movie_data.dropna(subset=["imdb_score"], inplace=True)
                movie_data.reset_index()

                genres = []
                genre_df = movie_data["genres"]
                my_genre_set = set()

                for i in genre_df:
                    my_genre_set.update(i.split("|"))

                print("LIST OF GENRES: ")
                print("-------")
                print("\n")
                for i in my_genre_set:
                    print(i)

                print("\nEnter a genre to see mean IMDB score of all films of that genre:")
                genre = input("Enter genre here: ")

                genre_imdb_df = movie_data[["genres", "imdb_score"]]
                is_genre_present = (genre_imdb_df.genres.str.contains(genre) == True)
                all_movie_genres = genre_imdb_df.loc[is_genre_present][["imdb_score"]]
                print("\nThe mean IMDB score of all films within " + genre + " genre is",
                      round(all_movie_genres["imdb_score"].mean(), 1))
                break

            elif option == 5:
                # MENU OPTION 5 - EARNINGS AND IMDB SCORES
                movie_data.dropna(inplace=True)
                print(movie_data.corr()["imdb_score"])
                sns.heatmap(movie_data.corr(),annot = True)
                pl.show()
                break

            elif option == 6:
                # MENU OPTION 6 - EXIT
                break
    except:
        print("\nInvalid input! 1Please try again.")


# In[ ]:




