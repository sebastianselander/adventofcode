from dataclasses import dataclass
from utils import *
from copy import deepcopy

boss_life, boss_dmg = nums(readfile(2015,22))

@dataclass
class Boss:
    life = boss_life
    dmg = boss_dmg

@dataclass
class Player:
    life = 50
    dmg = 0
    armor = 0
    mana = 500
    mana_spent = 0
    effects = []

@dataclass
class MagicMissiles:
    name = "MagicMissiles"
    mana = 53
    tick = 1
    def effect(self,player):
        if self.tick >= 0:
            player.dmg += 4
            self.tick -= 1

@dataclass
class Drain:
    name = "Drain"
    mana = 73
    tick = 1
    def effect(self,player):
        if self.tick >= 0:
            player.dmg += 2
            player.life += 2
            self.tick -= 1

@dataclass
class Shield:
    name = "Shield"
    mana = 113
    tick = 6
    def effect(self,player):
        if self.tick >= 0:
            player.armor = 7
            self.tick -= 1


@dataclass
class Poison:
    name = "Poison"
    mana = 173
    tick = 6
    def effect(self,player):
        if self.tick >= 0:
            player.dmg += 3
            self.tick -= 1

@dataclass
class Recharge:
    name = "Recharge"
    mana = 229
    tick = 5
    def effect(self,player):
        if self.tick >= 0:
            player.mana += 101
            self.tick -= 1

mana_spent = float("inf")

def play(dmg_per_turn, player,boss, player_turn):
    global mana_spent
    if player.mana_spent >= mana_spent:
        return
    player.armor = 0
    player.dmg = 0
    if boss.life <= 0:
        mana_spent = min(mana_spent,player.mana_spent)
        return
    if player.life <= 0:
        return
    for eff in player.effects:
        if eff.tick > 0:
            eff.effect(player)
    player.effects = [eff for eff in player.effects if eff.tick > 0]
    boss.life -= player.dmg
    if not player_turn:
        player.life -= max(1, boss.dmg - player.armor)
        play(dmg_per_turn, player, boss, not player_turn)
    else:
        player.life -= dmg_per_turn
        for spell in [MagicMissiles(), Drain(), Shield(), Poison(), Recharge()]:
            if player.mana >= spell.mana:
                names = [x.name for x in player.effects]
                if spell.name in names:
                    continue
                boss_ = deepcopy(boss)
                player_ = deepcopy(player)
                player_.mana -= spell.mana
                player_.mana_spent += spell.mana
                player_.effects.append(spell)
                play(dmg_per_turn, player_, boss_, not player_turn)
        return

play(0, Player(), Boss(), True)
print(mana_spent)
mana_spent = float("inf")
play(1, Player(), Boss(), True)
print(mana_spent)
