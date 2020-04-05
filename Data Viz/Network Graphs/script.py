import csv
import json
import time
import tweepy
from unidecode import unidecode

# You must use Python 2.7.x
# Rate limit chart for Twitter REST API - https://developer.twitter.com/en/docs/basics/rate-limiting.html

def loadKeys(key_file):
    # TODO: put your keys and tokens in the keys.json file,
    #       then implement this method for loading access keys and token from keys.json
    # rtype: str <api_key>, str <api_secret>, str <token>, str <token_secret>

    # Load keys here and replace the empty strings in the return statement with those keys
    data = json.load(open(key_file))

    return unidecode(data['api_key']), unidecode(data['api_secret']), unidecode(data['token']), unidecode(data['token_secret'])

# Q1.b.(i) - 5 points
def getPrimaryFriends(api, root_user, no_of_friends):
    # TODO: implement the method for fetching 'no_of_friends' primary friends of 'root_user'
    # rtype: list containing entries in the form of a tuple (root_user, friend)
    primary_friends = []

    api.wait_on_rate_limit=True
    api.wait_on_rate_limit_notify=True
    
    for friend in tweepy.Cursor(api.friends, screen_name = root_user, count = 20).items(no_of_friends):
        primary_friends.append(unidecode(friend.screen_name))
    primary_friends = [(root_user,c) for c in primary_friends]
    # Add code here to populate primary_friends
    return primary_friends

# Q1.b.(ii) - 7 points
def getNextLevelFriends(api, friends_list, no_of_friends):
    # TODO: implement the method for fetching 'no_of_friends' friends for each entry in friends_list
    # rtype: list containing entries in the form of a tuple (friends_list[i], friend)
    
    next_level_friends = []

    api.wait_on_rate_limit=True
    api.wait_on_rate_limit_notify=True
    
    for i in friends_list:
        next_level_friends.append(getPrimaryFriends(api, i, no_of_friends))
    next_level_friends = [a for b in next_level_friends for a in b]
    
    # Add code here to populate next_level_friends
    return next_level_friends

# Q1.b.(iii) - 7 points
def getNextLevelFollowers(api, followers_list, no_of_followers):
    # TODO: implement the method for fetching 'no_of_followers' followers for each entry in followers_list
    # rtype: list containing entries in the form of a tuple (follower, followers_list[i])
    api.wait_on_rate_limit=True
    api.wait_on_rate_limit_notify=True
    def getPrimaryFollowers(api, root_user, no_of_followers):
        primary_followers = []
        api.wait_on_rate_limit=True
        api.wait_on_rate_limit_notify=True        
        for follower in tweepy.Cursor(api.followers, screen_name = root_user, count = 20).items(no_of_followers):
            primary_followers.append(unidecode(follower.screen_name))
        primary_followers = [(c,root_user) for c in primary_followers]
        return primary_followers
    next_level_followers = []
    for i in followers_list:
        next_level_followers.append(getPrimaryFollowers(api, i, no_of_followers))
    next_level_followers = [a for b in next_level_followers for a in b]
    # Add code here to populate next_level_followers
    return next_level_followers

# Q1.b.(i),(ii),(iii) - 4 points
def GatherAllEdges(api, root_user, no_of_neighbours):
    # TODO:  implement this method for calling the methods getPrimaryFriends, getNextLevelFriends
    #        and getNextLevelFollowers. Use no_of_neighbours to specify the no_of_friends/no_of_followers parameter.
    #        NOT using the no_of_neighbours parameter may cause issues with grading.
    # rtype: list containing entries in the form of a tuple (Source, Target). Refer to the "Note(s)" in the
    #        Question doc to know what Source node and Target node of an edge is in the case of Followers and Friends.
    all_edges = []
    api.wait_on_rate_limit=True
    api.wait_on_rate_limit_notify=True
    friends_list = [a[1] for a in getPrimaryFriends(api, root_user, no_of_friends=no_of_neighbours)]
    followers_list = friends_list
    all_edges.append(getNextLevelFriends(api, friends_list, no_of_friends=no_of_neighbours))
    all_edges.append(getNextLevelFollowers(api, followers_list, no_of_followers=no_of_neighbours))
    all_edges = [a for b in all_edges for a in b]
    all_edges = [','.join(pair).replace(' ', '') for pair in all_edges]
    #Add code here to populate all_edges
    return all_edges

# Q1.b.(i),(ii),(iii) - 5 Marks
def writeToFile(data, output_file):
    # write data to output_file
    # rtype: None
    with open(output_file,'w') as result:
        csv_output=csv.writer(result,lineterminator = '\n')
        for row in data:
            csv_output.writerow((row,''))
    pass


"""
You may modify testSubmission()
for your testing purposes
but it will not be graded.

It is highly recommended that
you DO NOT put any code outside testSubmission().

Note that your code should work as expected
for any value of ROOT_USER.
"""

def testSubmission():
    KEY_FILE = 'keys.json'
    OUTPUT_FILE_GRAPH = 'graph.csv'
    NO_OF_NEIGHBOURS = 20
    ROOT_USER = 'PoloChau'

    api_key, api_secret, token, token_secret = loadKeys(KEY_FILE)

    auth = tweepy.OAuthHandler(api_key, api_secret)
    auth.set_access_token(token, token_secret)
    api = tweepy.API(auth)

    edges = GatherAllEdges(api, ROOT_USER, NO_OF_NEIGHBOURS)

    writeToFile(edges, OUTPUT_FILE_GRAPH)


if __name__ == '__main__':
    testSubmission()
