#!/usr/bin/python
import random
from sys import exit
from time import sleep
from collections import Counter
from operator import itemgetter
from itertools import groupby

options = ['Ones', 'Deuces', 'Treys', 'Fours', 'Fives', 'Sixes', 'Three-of-a-kind',
'Four-of-a-kind', 'Full House', 'Small Straight', 'Large Straight',
'Yahtzee', 'Chance']

def start_game():
	print "\nWith how many players would you like to play? (min is 1, max is 4)"
	nr_of_players = raw_input('> ')
	nr_of_players = int(nr_of_players)
	if nr_of_players < 1 or nr_of_players > 4:
		print "That's not a legitimate number of players..."
		exit(0)
	else:
		players = []
		for i in range(0, nr_of_players):
			player_nr = str(i + 1)
			print "What's the name of player " + player_nr+'?'
			name_iter = raw_input('> ')
			players.append(name_iter)
	return players

def create_score_frame():
	score_frame = [
	['Ones			', None],
	['Deuces			', None],
	['Treys			', None],
	['Fours			', None],
	['Fives			', None],
	['Sixes			', None],
	['Three-of-a-kind		', None],
	['Four-of-a-kind		', None],
	['Full House		', None],
	['Small Straight		', None],
	['Large Straight		', None],
	['Yahtzee			', None],
	['Chance			', None]
	]
	return score_frame


def current_state(frame):
	for i in range(0, len(frame)):
		print frame[i][0] + str(frame[i][1])


def print_dice_values(dice_values):
	for i in range(0, len(dice_values)):
		print dice_values[i][0] + str(dice_values[i][1])

def first_roll():
	dice_values = [
	['Dice 1:  ', random.randint(1, 6)],
	['Dice 2:  ', random.randint(1, 6)],
	['Dice 3:  ', random.randint(1, 6)],
	['Dice 4:  ', random.randint(1, 6)],
	['Dice 5:  ', random.randint(1, 6)]
	]
	print 'The values of your dice are:\n'
	print_dice_values(dice_values)
	return dice_values

def second_and_third_roll(dice_values):
	print "\nEnter the number(s) of the dice you'd like to roll again:"
	dice_to_roll_raw = raw_input('> ')
	dice_to_roll = [s for s in dice_to_roll_raw if s.isdigit()]
	dice_to_roll = map(int, dice_to_roll)
	dice_to_roll_ind = [i - 1 for i in dice_to_roll]
	for i in dice_to_roll_ind:
		dice_values[i][1] = random.randint(1, 6)
	sleep(1)
	print '\nThe new values of you dice are:'
	print_dice_values(dice_values)
	return dice_values


# Functie die voor een gegeven keuze bepaalt hoeveel punten er zijn behaald.
def determine_points(dice_values, choice):
	# voor het bepalen van de onderste helft hebben we de min en max van
	# het aantal keer dat een waarde voorkomt nodig (waarde zelf maakt niet uit)
	values = []
	for i in range(0, len(dice_values)):
		values.append(dice_values[i][1])
	value_occurence = Counter( values )
	value_occurence = value_occurence.items()
	occurences = []
	for i in range(0, len(value_occurence)):
		occurences.append( value_occurence[i][1])
	max_occurence = max(occurences)
	min_occurence = min(occurences)
	# voor de straten moeten we weten hoeveel opeenvolgende
	# waarde er in de values zitten
	values_for_straight = values
	values_for_straight.sort()
	values_for_straight = list(set(values_for_straight))
	ranges = []
	for k, g in groupby(enumerate(values_for_straight), lambda (i,x):i-x):
		ranges.append( map(itemgetter(1), g) )
	range_length = []
	for i in range(0, len(ranges)):
		range_length.append( len(ranges[i]) )
	most_consecutive = max(range_length)
	# bepalen van de punten onder een bepaalde waarde
	if choice == 'Ones':
		points = sum([ i for i in values if i == 1 ])
	elif choice == 'Deuces':
		points = sum([ i for i in values if i ==  2])
	elif choice == 'Treys':
		points = sum([ i for i in values if i ==  3])
	elif choice == 'Fours':
		points = sum([ i for i in values if i ==  4])
	elif choice == 'Fives':
		points = sum([ i for i in values if i ==  5])
	elif choice == 'Sixes':
		points = sum([ i for i in values if i ==  6])
	elif choice == 'Three-of-a-kind':
		if max_occurence < 3:
			points = 0
		else:
			points = sum(values)
	elif choice == "Four-of-a-kind":
		if max_occurence < 4:
			points = 0
		else:
			points = sum(values)
	elif choice == 'Full House':
		if max_occurence == 3 and min_occurence == 2:
			points = 25
		else:
			points = 0
	elif choice == "Small Straight":
		if most_consecutive > 3:
			points = 30
		else:
			points = 0
	elif choice == 'Large Straight':
		if most_consecutive == 5:
			points = 40
		else:
			points = 0
	elif choice == 'Yahtzee':
		if max_occurence == 5:
			points = 50
		else:
			points = 0
	elif choice == 'Chance':
			points = sum(values)
	else:
		print '''That's not a valid choice, make sure to exactly copy the choice name from the table'''
		points = None
	return points


def assign_score(dice_values, score_frame):
	print '\nThese are your scores so far:\n'
	current_state(score_frame)
	sleep(1)
	print '\nThis round your final dice values are:\n'
	print_dice_values(dice_values)
	chosen = False
	while chosen == False:
		print '\nTo which field would you like to assign the score?'
		field_to_assign = raw_input('> ')
		valid_option = False
		while valid_option == False:
			if field_to_assign not in options:
				'\n %s is not a valid choice, make sure to exactly copy the choice name from the table' % field_to_assign
				field_to_assign = raw_input('> ')
			else:
				for i in range(0,len(options)):
					if field_to_assign == options[i]:
						ind = i
				if score_frame[ind][1] == None:
					valid_option = True
				else:
					print '''%s has already been played, please select one that hasn't.''' % field_to_assign
					field_to_assign = raw_input('> ')
		points = determine_points(dice_values, field_to_assign)
		print '''\nThis would give you %s points, do you want to play %s (y/n)?\n''' % (points, field_to_assign)
		play = raw_input('> ')
		chose_to_play = False
		while chose_to_play == False:
			if play == 'n':
				chosen = False
				chose_to_play = True
			elif play == 'y':
				score_frame[ind][1] = points
				chosen = True
				chose_to_play = True
			else:
				print("Please choose between y or n only\n")
				play = raw_input('> ')
# wrapper functie dat in een keer een ronde speelt

def play_round(player, score_frame):
	print "\nIts %s's turn to play..." % player
	sleep(1)
	print "\nYour scores so far are:"
	current_state(score_frame)
	#
	sleep(1)
	print "\nHere we go:\n"
	sleep(1)
	dice = first_roll()
	dice = second_and_third_roll(dice)
	dice = second_and_third_roll(dice)
	#
	sleep(1)
	assign_score(dice, score_frame)

def get_final_score(score_frame):
	scores = []
	for i in range(0, 13):
		scores.append(score_frame[i][1])
	return sum(scores)

# alle functies samenvoegen tot het spel
players = start_game()

score_frames = {}
for i in range(0, len(players)):
	score_frames[i] = create_score_frame()

for i in range(0, 13):
	for j in range(0, len(players)):
		play_round(players[j], score_frames[j])

final_scores = []
for i in range(0, len(players)):
	final_scores.append(
		[ players[i], get_final_score(score_frames[i]) ]
		)

