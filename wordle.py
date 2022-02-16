print("Welcome to Wordle Cracker for people like us")
print("who suck in English vocabulary, but love technology")
print("***** Lingo.ai is warming up ****")
print("You need to enter indexes of green and brown characters one by one seperated by comma")
print("Indexes start from 1 to 5. If there is none (no characters), type 0 or just press enter")
print("Once you are done press (E) to exit")
print()
print()
print("There are 2 modes to use this (A)utomatic or (I)ntelligent")
print("Automatic will run on its own, but Intelligent will take your inputs in between")
mode = input("Press (A) for automatic or (I) for Intelligent mode")
#print(mode)
while ((mode!="A")and(mode!="a")and(mode!="I")and(mode!="i")):
    print("Try again")
    mode = input("Press (A) for automatic or (I) for Intelligent mode")

def initialize_():
    global database 
    global database_freq 
    global databse_list 
    global databse_list_freq 
    global one_list, two_list, three_list, four_list, five_list, databse_list_new, one_list_new,two_list_new,three_list_new,four_list_new,five_list_new
    
    database = "which, there, their, about, would, these, other, words, could, write, first, water, after, where, right, think, three, years, place, sound, great, again, still, every, small, found, those, never, under, might, while, house, world, below, asked, going, large, until, along, shall, being, often, earth, began, since, study, night, light, above, paper, parts, young, story, point, times, heard, whole, white, given, means, music, miles, thing, today, later, using, money, lines, order, group, among, learn, known, space, table, early, trees, short, hands, state, black, shown, stood, front, voice, kinds, makes, comes, close, power, lived, vowel, taken, built, heart, ready, quite, class, bring, round, horse, shows, piece, green, stand, birds, start, river, tried, least, field, whose, girls, leave, added, color, third, hours, moved, plant, doing, names, forms, heavy, ideas, cried, check, floor, begin, woman, alone, plane, spell, watch, carry, wrote, clear, named, books, child, glass, human, takes, party, build, seems, blood, sides, seven, mouth, solve, north, value, death, maybe, happy, tells, gives, looks, shape, lives, steps, areas, sense, speak, force, ocean, speed, women, metal, south, grass, scale, cells, lower, sleep, wrong, pages, ships, needs, rocks, eight, major, level, total, ahead, reach, stars, store, sight, terms, catch, works, board, cover, songs, equal, stone, waves, guess, dance, spoke, break, cause, radio, weeks, lands, basic, liked, trade, fresh, final, fight, meant, drive, spent, local, waxes, knows, train, bread, homes, teeth, coast, thick, brown, clean, quiet, sugar, facts, steel, forth, rules, notes, units, peace, month, verbs, seeds, helps, sharp, visit, woods, chief, walls, cross, wings, grown, cases, foods, crops, fruit, stick, wants, stage, sheep, nouns, plain, drink, bones, apart, turns, moves, touch, angle, based, range, marks, tired, older, farms, spend, shoes, goods, chair, twice, cents, empty, alike, style, broke, pairs, count, enjoy, score, shore, roots, paint, heads, shook, serve, angry, crowd, wheel, quick, dress, share, alive, noise, solid, cloth, signs, hills, types, drawn, worth, truck, piano, upper, loved, usual, faces, drove, cabin, boats, towns, proud, court, model, prime, fifty, plans, yards, prove, tools, price, sheet, smell, boxes, raise, match, truth, roads, threw, enemy, lunch, chart, scene, graph, doubt, guide, winds, block, grain, smoke, mixed, games, wagon, sweet, topic, extra, plate, title, knife, fence, falls, cloud, wheat, plays, enter, broad, steam, atoms, press, lying, basis, clock, taste, grows, thank, storm, agree, brain, track, smile, funny, beach, stock, hurry, saved, sorry, giant, trail, offer, ought, rough, daily, avoid, keeps, throw, allow, cream, laugh, edges, teach, frame, bells, dream, magic, occur, ended, chord, False, skill, holes, dozen, brave, apple, climb, outer, pitch, ruler, holds, fixed, costs, calls, blank, staff, labor, eaten, youth, tones, honor, globe, gases, doors, poles, loose, apply, tears, exact, brush, chest, layer, whale, minor, faith, tests, judge, items, worry, waste, hoped, strip, begun, aside, lakes, bound, depth, candy, event, worse, aware, shell, rooms, ranch, image, snake, aloud, dried, likes, motor, pound, knees, refer, fully, chain, shirt, flour, drops, spite, orbit, banks, shoot, curve, tribe, tight, blind, slept, shade, claim, flies, theme, queen, fifth, union, hence, straw, entry, issue, birth, feels, anger, brief, rhyme, glory, guard, flows, flesh, owned, trick, yours, sizes, noted, width, burst, route, lungs, uncle, bears, royal, kings, forty, trial, cards, brass, opera, chose, owner, vapor, beats, mouse, tough, wires, meter, tower, finds, inner, stuck, arrow, poems, label, swing, solar, truly, tense, beans, split, rises, weigh, hotel, stems, pride, swung, grade, digit, badly, boots, pilot, sales, swept, lucky, prize, stove, tubes, acres, wound, steep, slide, trunk, error, porch, slave, exist, faced, mines, marry, juice, raced, waved, goose, trust, fewer, favor, mills, views, joint, eager, spots, blend, rings, adult, index, nails, horns, balls, flame, rates, drill, trace, skins, waxed, seats, stuff, ratio, minds, dirty, silly, coins, hello, trips, leads, rifle, hopes, bases, shine, bench, moral, fires, meals, shake, shops, cycle, movie, slope, canoe, teams, folks, fired, bands, thumb, shout, canal, habit, reply, ruled, fever, crust, shelf, walks, midst, crack, print, tales, coach, stiff, flood, verse, awake, rocky, march, fault, swift, faint, civil, ghost, feast, blade, limit, germs, reads, ducks, dairy, worst, gifts, lists, stops, rapid, brick, claws, beads, beast, skirt, cakes, lions, frogs, tries, nerve, grand, armed, treat, honey, moist, legal, penny, crown, shock, taxes, sixty, altar, pulls, sport, drums, talks, dying, dates, drank, blows, lever, wages, proof, drugs, tanks, sings, tails, pause, herds, arose, hated, clues, novel, shame, burnt, races, flash, weary, heels, token, coats, spare, shiny, alarm, dimes, sixth, clerk, mercy, sunny, guest, float, shone, pipes, worms, bills, sweat, suits, smart, upset, rains, sandy, rainy, parks, sadly, fancy, rider, unity, bunch, rolls, crash, craft, newly, gates, hatch, paths, funds, wider, grace, grave, tides, admit, shift, sails, pupil, tiger, angel, cruel, agent, drama, urged, patch, nests, vital, sword, blame, weeds, screw, vocal, bacon, chalk, cargo, crazy, acted, goats, arise, witch, loves, queer, dwell, backs, ropes, shots, merry, phone, cheek, peaks, ideal, beard, eagle, creek, cries, ashes, stall, yield, mayor, opens, input, fleet, tooth, cubic, wives, burns, poets, apron, spear, organ, cliff, stamp, paste, rural, baked, chase, slice, slant, knock, noisy, sorts, stays, wiped, blown, piled, clubs, cheer, widow, twist, tenth, hides, comma, sweep, spoon, stern, crept, maple, deeds, rides, muddy, crime, jelly, ridge, drift, dusty, devil, tempo, humor, sends, steal, tents, waist, roses, reign, noble, cheap, dense, linen, geese, woven, posts, hired, wrath, salad, bowed, tires, shark, belts, grasp, blast, polar, fungi, tends, pearl, loads, jokes, veins, frost, hears, loses, hosts, diver, phase, toads, alert, tasks, seams, coral, focus, naked, puppy, jumps, spoil, quart, macro, fears, flung, spark, vivid, brook, steer, spray, decay, ports, socks, urban, goals, grant, minus, films, tunes, shaft, firms, skies, bride, wreck, flock, stare, hobby, bonds, dared, faded, thief, crude, pants, flute, votes, tonal, radar, wells, skull, hairs, argue, wears, dolls, voted, caves, cared, broom, scent, panel, fairy, olive, bends, prism, lamps, cable, peach, ruins, rally, schwa, lambs, sells, cools, draft, charm, limbs, brake, gazed, cubes, delay, beams, fetch, ranks, array, harsh, camel, vines, picks, naval, purse, rigid, crawl, toast, soils, sauce, basin, ponds, twins, wrist, fluid, pools, brand, stalk, robot, reeds, hoofs, buses, sheer, grief, bloom, dwelt, melts, risen, flags, knelt, fiber, roofs, freed, armor, piles, aimed, algae, twigs, lemon, ditch, drunk, rests, chill, slain, panic, cords, tuned, crisp, ledge, dived, swamp, clung, stole, molds, yarns, liver, gauge, breed, stool, gulls, awoke, gross, diary, rails, belly, trend, flask, stake, fried, draws, actor, handy, bowls, haste, scope, deals, knots, moons, essay, thump, hangs, bliss, dealt, gains, bombs, clown, palms, cones, roast, tidal, bored, chant, acids, dough, camps, swore, lover, hooks, males, cocoa, punch, award, reins, ninth, noses, links, drain, fills, nylon, lunar, pulse, flown, elbow, fatal, sites, moths, meats, foxes, mined, attic, fiery, mount, usage, swear, snowy, rusty, scare, traps, relax, react, valid, robin, cease, gills, prior, safer, polio, loyal, swell, salty, marsh, vague, weave, mound, seals, mules, virus, scout, acute, windy, stout, folds, seize, hilly, joins, pluck, stack, lords, dunes, burro, hawks, trout, feeds, scarf, halls, coals, towel, souls, elect, buggy, pumps, loans, spins, files, oxide, pains, photo, rival, flats, syrup, rodeo, sands, moose, pints, curly, comic, cloak, onion, clams, scrap, didst, couch, codes, fails, ounce, lodge, greet, gypsy, utter, paved, zones, fours, alley, tiles, bless, crest, elder, kills, yeast, erect, bugle, medal, roles, hound, snail, alter, ankle, relay, loops, zeros, bites, modes, debts, realm, glove, rayon, swims, poked, stray, lifts, maker, lumps, graze, dread, barns, docks, masts, pours, wharf, curse, plump, robes, seeks, cedar, curls, jolly, myths, cages, gloom, locks, pedal, beets, crows, anode, slash, creep, rowed, chips, fists, wines, cares, valve, newer, motel, ivory, necks, clamp, barge, blues, alien, frown, strap, crews, shack, gonna, saves, stump, ferry, idols, cooks, juicy, glare, carts, alloy, bulbs, lawns, lasts, fuels, oddly, crane, filed, weird, shawl, slips, troop, bolts, suite, sleek, quilt, tramp, blaze, atlas, odors, scrub, crabs, probe, logic, adobe, exile, rebel, grind, sting, spine, cling, desks, grove, leaps, prose, lofty, agony, snare, tusks, bulls, moods, humid, finer, dimly, plank, china, pines, guilt, sacks, brace, quote, lathe, gaily, fonts, scalp, adopt, foggy, ferns, grams, clump, perch, tumor, teens, crank, fable, hedge, genes, sober, boast, tract, cigar, unite, owing, thigh, haiku, swish, dikes, wedge, booth, eased, frail, cough, tombs, darts, forts, choir, pouch, pinch, hairy, buyer, torch, vigor, waltz, heats, herbs, users, flint, click, madam, bleak, blunt, aided, lacks, masks, waded, risks, nurse, chaos, sewed, cured, ample, lease, steak, sinks, merit, bluff, bathe, gleam, bonus, colts, shear, gland, silky, skate, birch, anvil, sleds, groan, maids, meets, speck, hymns, hints, drown, bosom, slick, quest, coils, spied, snows, stead, snack, plows, blond, tamed, thorn, waits, glued, banjo, tease, arena, bulky, carve, stunt, warms, shady, razor, folly, leafy, notch, fools, otter, pears, flush, genus, ached, fives, flaps, spout, smote, fumes, adapt, cuffs, tasty, stoop, clips, disks, sniff, lanes, brisk, imply, demon, super, furry, raged, growl, texts, hardy, stung, typed, hates, wiser, timid, serum, beaks, rotor, casts, baths, glide, plots, trait, resin, slums, lyric, puffs, decks, brood, mourn, aloft, abuse, whirl, edged, ovary, quack, heaps, slang, await, civic, saint, bevel, sonar, aunts, packs, froze, tonic, corps, swarm, frank, repay, gaunt, wired, niece, cello, needy, chuck, stony, media, surge, hurts, repel, husky, dated, hunts, mists, exert, dries, mates, sworn, baker, spice, oasis, boils, spurs, doves, sneak, paces, colon, siege, strum, drier, cacao, humus, bales, piped, nasty, rinse, boxer, shrub, amuse, tacks, cited, slung, delta, laden, larva, rents, yells, spool, spill, crush, jewel, snaps, stain, kicks, tying, slits, rated, eerie, smash, plums, zebra, earns, bushy, scary, squad, tutor, silks, slabs, bumps, evils, fangs, snout, peril, pivot, yacht, lobby, jeans, grins, viola, liner, comet, scars, chops, raids, eater, slate, skips, soles, misty, urine, knobs, sleet, holly, pests, forks, grill, trays, pails, borne, tenor, wares, carol, woody, canon, wakes, kitty, miner, polls, shaky, nasal, scorn, chess, taxis, crate, shyly, tulip, forge, nymph, budge, lowly, abide, depot, oases, asses, sheds, fudge, pills, rivet, thine, groom, lanky, boost, broth, heave, gravy, beech, timed, quail, inert, gears, chick, hinge, trash, clash, sighs, renew, bough, dwarf, slows, quill, shave, spore, sixes, chunk, madly, paced, braid, fuzzy, motto, spies, slack, mucus, magma, awful, discs, erase, posed, asset, cider, taper, theft, churn, satin, slots, taxed, bully, sloth, shale, tread, raked, curds, manor, aisle, bulge, loins, stair, tapes, leans, bunks, squat, towed, lance, panes, sakes, heirs, caste, dummy, pores, fauna, crook, poise, epoch, risky, warns, fling, berry, grape, flank, drags, squid, pelts, icing, irony, irons, barks, whoop, choke, diets, whips, tally, dozed, twine, kites, bikes, ticks, riots, roars, vault, looms, scold, blink, dandy, pupae, sieve, spike, ducts, lends, pizza, brink, widen, plumb, pagan, feats, bison, soggy, scoop, argon, nudge, skiff, amber, sexes, rouse, salts, hitch, exalt, leash, dined, chute, snort, gusts, melon, cheat, reefs, llama, lasso, debut, quota, oaths, prone, mixes, rafts, dives, stale, inlet, flick, pinto, brows, untie, batch, greed, chore, stirs, blush, onset, barbs, volts, beige, swoop, paddy, laced, shove, jerky, poppy, leaks, fares, dodge, godly, squaw, affix, brute, nicer, undue, snarl, merge, doses, showy, daddy, roost, vases, swirl, petty, colds, curry, cobra, genie, flare, messy, cores, soaks, ripen, whine, amino, plaid, spiny, mowed, baton, peers, vowed, pious, swans, exits, afoot, plugs, idiom, chili, rites, serfs, cleft, berth, grubs, annex, dizzy, hasty, latch, wasps, mirth, baron, plead, aloof, aging, pixel, bared, mummy, hotly, auger, buddy, chaps, badge, stark, fairs, gully, mumps, emery, filly, ovens, drone, gauze, idiot, fussy, annoy, shank, gouge, bleed, elves, roped, unfit, baggy, mower, scant, grabs, fleas, lousy, album, sawed, cooky, murky, infer, burly, waged, dingy, brine, kneel, creak, vanes, smoky, spurt, combs, easel, laces, humps, rumor, aroma, horde, swiss, leapt, opium, slime, afire, pansy, mares, soaps, husks, snips, hazel, lined, cafes, naive, wraps, sized, piers, beset, agile, tongs, steed, fraud, booty, valor, downy, witty, mossy, psalm, scuba, tours, polka, milky, gaudy, shrug, tufts, wilds, laser, truss, hares, creed, lilac, siren, tarry, bribe, swine, muted, flips, cures, sinew, boxed, hoops, gasps, hoods, niche, yucca, glows, sewer, whack, fuses, gowns, droop, bucks, pangs, mails, whisk, haven, clasp, sling, stint, urges, champ, piety, chirp, pleat, posse, sunup, menus, howls, quake, knack, plaza, fiend, caked, bangs, erupt, poker, olden, cramp, voter, poses, manly, slump, fined, grips, gaped, purge, hiked, maize, fluff, strut, sloop, prowl, roach, cocks, bland, dials, plume, slaps, soups, dully, wills, foams, solos, skier, eaves, totem, fused, latex, veils, mused, mains, myrrh, racks, galls, gnats, bouts, sisal, shuts, hoses, dryly, hover, gloss, seeps, denim, putty, guppy, leaky, dusky, filth, oboes, spans, fowls, adorn, glaze, haunt, dares, obeys, bakes, abyss, smelt, gangs, aches, trawl, claps, undid, spicy, hoist, fades, vicar, acorn, pussy, gruff, musty, tarts, snuff, hunch, truce, tweed, dryer, loser, sheaf, moles, lapse, tawny, vexed, autos, wager, domes, sheen, clang, spade, sowed, broil, slyly, studs, grunt, donor, slugs, aspen, homer, croak, tithe, halts, avert, havoc, hogan, glint, ruddy, jeeps, flaky, ladle, taunt, snore, fines, props, prune, pesos, radii, pokes, tiled, daisy, heron, villa, farce, binds, cites, fixes, jerks, livid, waked, inked, booms, chews, licks, hyena, scoff, lusty, sonic, smith, usher, tucks, vigil, molts, sects, spars, dumps, scaly, wisps, sores, mince, panda, flier, axles, plied, booby, patio, rabbi, petal, polyp, tints, grate, troll, tolls, relic, phony, bleat, flaws, flake, snags, aptly, drawl, ulcer, soapy, bossy, monks, crags, caged, twang, diner, taped, cadet, grids, spawn, guile, noose, mores, girth, slimy, aides, spasm, burrs, alibi, lymph, saucy, muggy, liter, joked, goofy, exams, enact, stork, lured, toxic, omens, nears, covet, wrung, forum, venom, moody, alder, sassy, flair, guild, prays, wrens, hauls, stave, tilts, pecks, stomp, gales, tempt, capes, mesas, omits, tepee, harry, wring, evoke, limes, cluck, lunge, highs, canes, giddy, lithe, verge, khaki, queue, loath, foyer, outdo, fared, deter, crumb, astir, spire, jumpy, extol, buoys, stubs, lucid, thong, afore, whiff, maxim, hulls, clogs, slats, jiffy, arbor, cinch, igloo, goody, gazes, dowel, calms, bitch, scowl, gulps, coded, waver, mason, lobes, ebony, flail, isles, clods, dazed, adept, oozed, sedan, clays, warts, ketch, skunk, manes, adore, sneer, mango, fiord, flora, roomy, minks, thaws, watts, freer, exult, plush, paled, twain, clink, scamp, pawed, grope, bravo, gable, stink, sever, waned, rarer, regal, wards, fawns, babes, unify, amend, oaken, glade, visor, hefty, nines, throb, pecan, butts, pence, sills, jails, flyer, saber, nomad, miter, beeps, domed, gulfs, curbs, heath, moors, aorta, larks, tangy, wryly, cheep, rages, evade, lures, freak, vogue, tunic, slams, knits, dumpy, mania, spits, firth, hikes, trots, nosed, clank, dogma, bloat, balsa, graft, middy, stile, keyed, finch, sperm, chaff, wiles, amigo, copra, amiss, eying, twirl, lurch, popes, chins, smock, tines, guise, grits, junks, shoal, cache, tapir, atoll, deity, toils, spree, mocks, scans, shorn, revel, raven, hoary, reels, scuff, mimic, weedy, corny, truer, rouge, ember, floes, torso, wipes, edict, sulky, recur, groin, baste, kinks, surer, piggy, moldy, franc, liars, inept, gusty, facet, jetty, equip, leper, slink, soars, cater, dowry, sided, yearn, decoy, taboo, ovals, heals, pleas, beret, spilt, gayly, rover, endow, pygmy, carat, abbey, vents, waken, chimp, fumed, sodas, vinyl, clout, wades, mites, smirk, bores, bunny, surly, frock, foray, purer, milks, query, mired, blare, froth, gruel, navel, paler, puffy, casks, grime, derby, mamma, gavel, teddy, vomit, moans, allot, defer, wield, viper, louse, erred, hewed, abhor, wrest, waxen, adage, ardor, stabs, pored, rondo, loped, fishy, bible, hires, foals, feuds, jambs, thuds, jeers, knead, quirk, rugby, expel, greys, rigor, ester, lyres, aback, glues, lotus, lurid, rungs, hutch, thyme, valet, tommy, yokes, epics, trill, pikes, ozone, caper, chime, frees, famed, leech, smite, neigh, erode, robed, hoard, salve, conic, gawky, craze, jacks, gloat, mushy, rumps, fetus, wince, pinks, shalt, toots, glens, cooed, rusts, stews, shred, parka, chugs, winks, clots, shrew, booed, filmy, juror, dents, gummy, grays, hooky, butte, dogie, poled, reams, fifes, spank, gayer, tepid, spook, taint, flirt, rogue, spiky, opals, miser, cocky, coyly, balmy, slosh, brawl, aphid, faked, hydra, brags, chide, yanks, allay, video, altos, eases, meted, chasm, longs, excel, taffy, impel, savor, koala, quays, dawns, proxy, clove, duets, dregs, tardy, briar, grimy, ultra, meaty, halve, wails, suede, mauve, envoy, arson, coves, gooey, brews, sofas, chums, amaze, zooms, abbot, halos, scour, suing, cribs, sagas, enema, wordy, harps, coupe, molar, flops, weeps, mints, ashen, felts, askew, munch, mewed, divan, vices, jumbo, blobs, blots, spunk, acrid, topaz, cubed, clans, flees, slurs, gnaws, welds, fords, emits, agate, pumas, mends, darks, dukes, plies, canny, hoots, oozes, lamed, fouls, clefs, nicks, mated, skims, brunt, tuber, tinge, fates, ditty, thins, frets, eider, bayou, mulch, fasts, amass, damps, morns, friar, palsy, vista, croon, conch, udder, tacos, skits, mikes, quits, preen, aster, adder, elegy, pulpy, scows, baled, hovel, lavas, crave, optic, welts, busts, knave, razed, shins, totes, scoot, dears, crock, mutes, trims, skein, doted, shuns, veers, fakes, yoked, wooed, hacks, sprig, wands, lulls, seers, snobs, nooks, pined, perky, mooed, frill, dines, booze, tripe, prong, drips, odder, levee, antic, sidle, pithy, corks, yelps, joker, fleck, buffs, scram, tiers, bogey, doled, irate, vales, coped, hails, elude, bulks, aired, vying, stags, strew, cocci, pacts, scabs, silos, dusts, yodel, terse, jaded, baser, jibes, foils, sways, forgo, slays, preys, treks, quell, peeks, assay, lurks, eject, boars, trite, belch, gnash, wanes, lutes, whims, dosed, chewy, snipe, umbra, teems, dozes, kelps, upped, brawn, doped, shush, rinds, slush, moron, voile, woken, fjord, sheik, jests, kayak, slews, toted, saner, drape, patty, raves, sulfa, grist, skied, vixen, civet, vouch, tiara, homey, moped, runts, serge, kinky, rills, corns, brats, pries, amble, fries, loons, tsars, datum, musky, pigmy, gnome, ravel, ovule, icily, liken, lemur, frays, silts, sifts, plods, ramps, tress, earls, dudes, waive, karat, jolts, peons, beers, horny, pales, wreak, lairs, lynch, stank, swoon, idler, abort, blitz, ensue, atone, bingo, roves, kilts, scald, adios, cynic, dulls, memos, elfin, dales, peels, peals, bares, sinus, crone, sable, hinds, shirk, enrol, wilts, roams, duped, cysts, mitts, safes, spats, coops, filet, knell, refit, covey, punks, kilns, fitly, abate, talcs, heeds, duels, wanly, ruffs, gauss, lapel, jaunt, whelp, cleat, gauzy, dirge, edits, wormy, moats, smear, prods, bowel, frisk, vests, bayed, rasps, tames, delve, embed, befit, wafer, ceded, novas, feign, spews, larch, huffs, doles, mamas, hulks, pried, brims, irked, aspic, swipe, mealy, skimp, bluer, slake, dowdy, penis, brays, pupas, egret, flunk, phlox, gripe, peony, douse, blurs, darns, slunk, lefts, chats, inane, vials, stilt, rinks, woofs, wowed, bongs, frond, ingot, evict, singe, shyer, flied, slops, dolts, drool, dells, whelk, hippy, feted, ether, cocos, hives, jibed, mazes, trios, sirup, squab, laths, leers, pasta, rifts, lopes, alias, whirs, diced, slags, lodes, foxed, idled, prows, plait, malts, chafe, cower, toyed, chefs, keels, sties, racer, etude, sucks, sulks, micas, czars, copse, ailed, abler, rabid, golds, croup, snaky, visas, palls, mopes, boned, wispy, raved, swaps, junky, doily, pawns, tamer, poach, baits, damns, gumbo, daunt, prank, hunks, buxom, heres, honks, stows, unbar, idles, routs, sages, goads, remit, copes, deign, culls, girds, haves, lucks, stunk, dodos, shams, snubs, icons, usurp, dooms, hells, soled, comas, paves, maths, perks, limps, wombs, blurb, daubs, cokes, sours, stuns, cased, musts, coeds, cowed, aping, zoned, rummy, fetes, skulk, quaff, rajah, deans, reaps, galas, tills, roved, kudos, toned, pared, scull, vexes, punts, snoop, bails, dames, hazes, lores, marts, voids, ameba, rakes, adzes, harms, rears, satyr, swill, hexes, colic, leeks, hurls, yowls, ivies, plops, musks, papaw, jells, bused, cruet, bided, filch, zests, rooks, laxly, rends, loams, basks, sires, carps, pokey, flits, muses, bawls, shuck, viler, lisps, peeps, sorer, lolls, prude, diked, floss, flogs, scums, dopes, bogie, pinky, leafs, tubas, scads, lowed, yeses, biked, qualm, evens, caned, gawks, whits, wooly, gluts, romps, bests, dunce, crony, joist, tunas, boner, malls, parch, avers, crams, pares, dally, bigot, kales, flays, leach, gushy, pooch, huger, slyer, golfs, mires, flues, loafs, arced, acnes, neons, fiefs, dints, dazes, pouts, cored, yules, lilts, beefs, mutts, fells, cowls, spuds, lames, jawed, dupes, deads, bylaw, noons, nifty, clued, vireo, gapes, metes, cuter, maims, droll, cupid, mauls, sedge, papas, wheys, eking, loots, hilts, meows, beaus, dices, peppy, riper, fogey, gists, yogas, gilts, skews, cedes, zeals, alums, okays, elope, grump, wafts, soots, blimp, hefts, mulls, hosed, cress, doffs, ruder, pixie, waifs, ousts, pucks, biers, gulch, suets, hobos, lints, brans, teals, garbs, pewee, helms, turfs, quips, wends, banes, napes, icier, swats, bagel, hexed, ogres, goner, gilds, pyres, lards, bides, paged, talon, flout, medic, veals, putts, dirks, dotes, tippy, blurt, piths, acing, barer, whets, gaits, wools, dunks, heros, swabs, dirts, jutes, hemps, surfs, okapi, chows, shoos, dusks, parry, decal, furls, cilia, sears, novae, murks, warps, slues, lamer, saris, weans, purrs, dills, togas, newts, meany, bunts, razes, goons, wicks, ruses, vends, geode, drake, judos, lofts, pulps, lauds, mucks, vises, mocha, oiled, roman, ethyl, gotta, fugue, smack, gourd, bumpy, radix, fatty, borax, cubit, cacti, gamma, focal, avail, papal, golly, elite, versa, billy, adieu, annum, howdy, rhino, norms, bobby, axiom, setup, yolks, terns, mixer, genre, knoll, abode, junta, gorge, combo, alpha, overt, kinda, spelt, prick, nobly, ephod, audio, modal, veldt, warty, fluke, bonny, bream, rosin, bolls, doers, downs, beady, motif, humph, fella, mould, crepe, kerns, aloha, glyph, azure, riser, blest, locus, lumpy, beryl, wanna, brier, tuner, rowdy, mural, timer, canst, krill, quoth, lemme, triad, tenon, amply, deeps, padre, leant, pacer, octal, dolly, trans, sumac, foamy, lolly, giver, quipu, codex, manna, unwed, vodka, ferny, salon, duple, boron, revue, crier, alack, inter, dilly, whist, cults, spake, reset, loess, decor, mover, verve, ethic, gamut, lingo, dunno, align, sissy, incur, reedy, avant, piper, waxer, calyx, basil, coons, seine, piney, lemma, trams, winch, whirr, saith, ionic, heady, harem, tummy, sally, shied, dross, farad, saver, tilde, jingo, bower, serif, facto, belle, inset, bogus, caved, forte, sooty, bongo, toves, credo, basal, yella, aglow, glean, gusto, hymen, ethos, terra, brash, scrip, swash, aleph, tinny, itchy, wanta, trice, jowls, gongs, garde, boric, twill, sower, henry, awash, libel, spurn, sabre, rebut, penal, obese, sonny, quirt, mebbe, tacit, greek, xenon, hullo, pique, roger, negro, hadst, gecko, beget, uncut, aloes, louis, quint, clunk, raped, salvo, diode, matey, hertz, xylem, kiosk, apace, cawed, peter, wench, cohos, sorta, gamba, bytes, tango, nutty, axial, aleck, natal, clomp, gored, siree, bandy, gunny, runic, whizz, rupee, fated, wiper, bards, briny, staid, hocks, ochre, yummy, gents, soupy, roper, swath, cameo, edger, spate, gimme, ebbed, breve, theta, deems, dykes, servo, telly, tabby, tares, blocs, welch, ghoul, vitae, cumin, dinky, bronc, tabor, teeny, comer, borer, sired, privy, mammy, deary, gyros, sprit, conga, quire, thugs, furor, bloke, runes, bawdy, cadre, toxin, annul, egged, anion, nodes, picky, stein, jello, audit, echos, fagot, letup, eyrie, fount, caped, axons, amuck, banal, riled, petit, umber, miler, fibre, agave, bated, bilge, vitro, feint, pudgy, mater, manic, umped, pesky, strep, slurp, pylon, puree, caret, temps, newel, yawns, seedy, treed, coups, rangy, brads, mangy, loner, circa, tibia, afoul, mommy, titer, carne, kooky, motes, amity, suave, hippo, curvy, samba, newsy, anise, imams, tulle, aways, liven, hallo, wales, opted, canto, idyll, bodes, curio, wrack, hiker, chive, yokel, dotty, demur, cusps, specs, quads, laity, toner, decry, writs, saute, clack, aught, logos, tipsy, natty, ducal, bidet, bulgy, metre, lusts, unary, goeth, baler, sited, shies, hasps, brung, holed, swank, looky, melee, huffy, loamy, pimps, titan, binge, shunt, femur, libra, seder, honed, annas, coypu, shims, zowie, jihad, savvy, nadir, basso, monic, maned, mousy, omega, laver, prima, picas, folio, mecca, reals, troth, testy, balky, crimp, chink, abets, splat, abaci, vaunt, cutie, pasty, moray, levis, ratty, islet, joust, motet, viral, nukes, grads, comfy, voila, woozy, blued, whomp, sward, metro, skeet, chine, aerie, bowie, tubby, emirs, coati, unzip, slobs, trike, funky, ducat, dewey, skoal, wadis, oomph, taker, minim, getup, stoic, synod, runty, flyby, braze, inlay, venue, louts, peaty, orlon, humpy, radon, beaut, raspy, unfed, crick, nappy, vizor, yipes, rebus, divot, kiwis, vetch, squib, sitar, kiddo, dyers, cotta, matzo, lager, zebus, crass, dacha, kneed, dicta, fakir, knurl, runny, unpin, julep, globs, nudes, sushi, tacky, stoke, kaput, butch, hulas, croft, achoo, genii, nodal, outgo, spiel, viols, fetid, cagey, fudgy, epoxy, leggy, hanky, lapis, felon, beefy, coots, melba, caddy, segue, betel, frizz, drear, kooks, turbo, hoagy, moult, helix, zonal, arias, nosey, paean, lacey, banns, swain, fryer, retch, tenet, gigas, whiny, ogled, rumen, begot, cruse, abuts, riven, balks, sines, sigma, abase, ennui, gores, unset, augur, sated, odium, latin, dings, moire, scion, henna, kraut, dicks, lifer, prigs, bebop, gages, gazer, fanny, gibes, aural, tempi, hooch, rapes, snuck, harts, techs, emend, ninny, guava, scarp, liege, tufty, sepia, tomes, carob, emcee, prams, poser, verso, hubba, joule, baize, blips, scrim, cubby, clave, winos, rearm, liens, lumen, chump, nanny, trump, fichu, chomp, homos, purty, maser, woosh, patsy, shill, rusks, avast, swami, boded, ahhhh, lobed, natch, shish, tansy, snoot, payer, altho, sappy, laxer, hubby, aegis, riles, ditto, jazzy, dingo, quasi, septa, peaky, lorry, heerd, bitty, payee, seamy, apses, imbue, belie, chary, spoof, phyla, clime, babel, wacky, sumps, skids, khans, crypt, inure, nonce, outen, faire, hooey, anole, kazoo, calve, limbo, argot, ducky, faker, vibes, gassy, unlit, nervy, femme, biter, fiche, boors, gaffe, saxes, recap, synch, facie, dicey, ouija, hewer, legit, gurus, edify, tweak, caron, typos, rerun, polly, surds, hamza, nulls, hater, lefty, mogul, mafia, debug, pates, blabs, splay, talus, porno, moola, nixed, kilos, snide, horsy, gesso, jaggy, trove, nixes, creel, pater, iotas, cadge, skyed, hokum, furze, ankhs, curie, nutsy, hilum, remix, angst, burls, jimmy, veiny, tryst, codon, befog, gamed, flume, axman, doozy, lubes, rheas, bozos, butyl, kelly, mynah, jocks, donut, avian, wurst, chock, quash, quals, hayed, bombe, cushy, spacy, puked, leery, thews, prink, amens, tesla, intro, fiver, frump, capos, opine, coder, namer, jowly, pukes, haled, chard, duffs, bruin, reuse, whang, toons, frats, silty, telex, cutup, nisei, neato, decaf, softy, bimbo, adlib, loony, shoed, agues, peeve, noway, gamey, sarge, reran, epact, potty, coned, upend, narco, ikats, whorl, jinks, tizzy, weepy, posit, marge, vegan, clops, numbs, reeks, rubes, rower, biped, tiffs, hocus, hammy, bunco, fixit, tykes, chaws, yucky, hokey, resew, maven, adman, scuzz, slogs, souse, nacho, mimed, melds, boffo, debit, pinup, vagus, gulag, randy, bosun, educe, faxes, auras, pesto, antsy, betas, fizzy, dorky, snits, moxie, thane, mylar, nobby, gamin, gouty, esses, goyim, paned, druid, jades, rehab, gofer, tzars, octet, homed, socko, dorks, eared, anted, elide, fazes, oxbow, dowse, situs, macaw, scone, drily, hyper, salsa, mooch, gated, unjam, lipid, mitre, venal, knish, ritzy, divas, torus, mange, dimer, recut, meson, wined, fends, phage, fiats, caulk, cavil, panty, roans, bilks, hones, botch, estop, sully, sooth, gelds, ahold, raper, pager, fixer, infix, hicks, tuxes, plebe, twits, abash, twixt, wacko, primp, nabla, girts, miffs, emote, xerox, rebid, shahs, rutty, grout, grift, deify, biddy, kopek, semis, bries, acmes, piton, hussy, torts, disco, whore, boozy, gibed, vamps, amour, soppy, gonzo, durst, wader, tutus, perms, catty, glitz, brigs, nerds, barmy, gizmo, owlet, sayer, molls, shard, whops, comps, corer, colas, matte, droid, ploys, vapid, cairn, deism, mixup, yikes, prosy, raker, flubs, whish, reify, craps, shags, clone, hazed, macho, recto, refix, drams, biker, aquas, porky, doyen, exude, goofs, divvy, noels, jived, hulky, cager, harpy, oldie, vivas, admix, codas, zilch, deist, orcas, retro, pilaf, parse, rants, zingy, toddy, chiff, micro, veeps, girly, nexus, demos, bibbs, antes, lulus, gnarl, zippy, ivied, epees, wimps, tromp, grail, yoyos, poufs, hales, roust, cabal, rawer, pampa, mosey, kefir, burgs, unmet, cuspy, boobs, boons, hypes, dynes, nards, lanai, yogis, sepal, quark, toked, prate, ayins, hawed, swigs, vitas, toker, doper, bossa, linty, foist, mondo, stash, kayos, twerp, zesty, capon, wimpy, rewed, fungo, tarot, frosh, kabob, pinko, redid, mimeo, heist, tarps, lamas, sutra, dinar, whams, busty, spays, mambo, nabob, preps, odour, cabby, conks, sluff, dados, houri, swart, balms, gutsy, faxed, egads, pushy, retry, agora, drubs, daffy, chits, mufti, karma, lotto, toffs, burps, deuce, zings, kappa, clads, doggy, duper, scams, ogler, mimes, throe, zetas, waled, promo, blats, muffs, oinks, viand, coset, finks, faddy, minis, snafu, sauna, usury, muxes, craws, stats, condo, coxes, loopy, dorms, ascot, dippy, execs, dopey, envoi, umpty, gismo, fazed, strop, jives, slims, batik, pings, sonly, leggo, pekoe, prawn, luaus, campy, oodle, prexy, proms, touts, ogles, tweet, toady, naiad, hider, nuked, fatso, sluts, obits, narcs, tyros, delis, wooer, hyped, poset, byway, texas, scrod, avows, futon, torte, tuple, carom, kebab, tamps, jilts, duals, artsy, repro, modem, toped, psych, sicko, klutz, tarns, coxed, drays, cloys, anded, piker, aimer, suras, limos, flack, hapax, dutch, mucky, shire, klieg, staph, layup, tokes, axing, toper, duvet, cowry, profs, blahs, addle, sudsy, batty, coifs, suety, gabby, hafta, pitas, gouda, deice, taupe, topes, duchy, nitro, carny, limey, orals, hirer, taxer, roils, ruble, elate, dolor, wryer, snots, quais, coked, gimel, gorse, minas, goest, agape, manta, jings, iliac, admen, offen, cills, offal, lotta, bolas, thwap, alway, boggy, donna, locos, belay, gluey, bitsy, mimsy, hilar, outta, vroom, fetal, raths, renal, dyads, crocs, vires, culpa, kivas, feist, teats, thats, yawls, whens, abaca, ohhhh, aphis, fusty, eclat, perdu, mayst, exeat, molly, supra, wetly, plasm, buffa, semen, pukka, tagua, paras, stoat, secco, carte, haute, molal, shads, forma, ovoid, pions, modus, bueno, rheum, scurf, parer, ephah, doest, sprue, flams, molto, dieth, choos, miked, bronx, goopy, bally, plumy, moony, morts, yourn, bipod, spume, algal, ambit, mucho, spued, dozer, harum, groat, skint, laude, thrum, pappy, oncet, rimed, gigue, limed, plein, redly, humpf, lites, seest, grebe, absit, thanx, pshaw, yawps, plats, payed, areal, tilth, youse, gwine, thees, watsa, lento, spitz, yawed, gipsy, sprat, cornu, amahs, blowy, wahoo, lubra, mecum, whooo, coqui, sabra, edema, mrads, dicot, astro, kited, ouzel, didos, grata, bonne, axmen, klunk, summa, laves, purls, yawny, teary, masse, largo, bazar, pssst, sylph, lulab, toque, fugit, plunk, ortho, lucre, cooch, whipt, folky, tyres, wheee, corky, injun, solon, didot, kerfs, rayed, wassa, chile, begat, nippy, litre, magna, rebox, hydro, milch, brent, gyves, lazed, feued, mavis, inapt, baulk, casus, scrum, wised, fossa, dower, kyrie, bhoys, scuse, feuar, ohmic, juste, ukase, beaux, tusky, orate, musta, lardy, intra, quiff, epsom, neath, ocher, tared, homme, mezzo, corms, psoas, beaky, terry, infra, spivs, tuans, belli, bergs, anima, weirs, mahua, scops, manse, titre, curia, kebob, cycad, talky, fucks, tapis, amide, dolce, sloes, jakes, russe, blash, tutti, pruta, panga, blebs, tench, swarf, herem, missy, merse, pawky, limen, vivre, chert, unsee, tiros, brack, foots, welsh, fosse, knops, ileum, noire, firma, podgy, laird, thunk, shute, rowan, shoji, poesy, uncap, fames, glees, costa, turps, fores, solum, imago, byres, fondu, coney, polis, dictu, kraal, sherd, mumbo, wroth, chars, unbox, vacuo, slued, weest, hades, wiled, syncs, muser, excon, hoars, sibyl, passe, joeys, lotsa, lepta, shays, bocks, endue, darer, nones, ileus, plash, busby, wheal, buffo, yobbo, biles, poxes, rooty, licit, terce, bromo, hayey, dweeb, imbed, saran, bruit, punky, softs, biffs, loppy, agars, aquae, livre, biome, bunds, shews, diems, ginny, degum, polos, desex, unman, dungy, vitam, wedgy, glebe, apers, ridgy, roids, wifey, vapes, whoas, bunko, yolky, ulnas, reeky, bodge, brant, davit, deque, liker, jenny, tacts, fulls, treap, ligne, acked, refry, vower, aargh, churl, momma, gaols, whump, arras, marls, tiler, grogs, memes, midis, tided, haler, duces, twiny, poste, unrig, prise, drabs, quids, facer, spier, baric, geoid, remap, trier, gunks, steno, stoma, airer, ovate, torah, apian, smuts, pocks, yurts, exurb, defog, nuder, bosky, nimbi, mothy, joyed, labia, pards, jammy, bigly, faxer, hoppy, nurbs, cotes, dishy, vised, celeb, pismo, casas, withs, dodgy, scudi, mungs, muons, ureas, ioctl, unhip, krone, sager, verst, expat, gronk, uvula, shawm, bilgy, braes, cento, webby, lippy, gamic, lordy, mazed, tings, shoat, faery, wirer, diazo, carer, rater, greps, rente, zloty, viers, unapt, poops, fecal, kepis, taxon, eyers, wonts, spina, stoae, yenta, pooey, buret, japan, bedew, hafts, selfs, oared, herby, pryer, oakum, dinks, titty, sepoy, penes, fusee, winey, gimps, nihil, rille, giber, ousel, umiak, cuppy, hames, shits, azine, glads, tacet, bumph, coyer, honky, gamer, gooky, waspy, sedgy, bents, varia, djinn, junco, pubic, wilco, lazes, idyls, lupus, rives, snood, schmo, spazz, finis, noter, pavan, orbed, bates, pipet, baddy, goers, shako, stets, sebum, seeth, lobar, raver, ajuga, riced, velds, dribs, ville, dhows, unsew, halma, krona, limby, jiffs, treys, bauds, pffft, mimer, plebs, caner, jiber, cuppa, washy, chuff, unarm, yukky, styes, waker, flaks, maces, rimes, gimpy, guano, liras, kapok, scuds, bwana, oring, aider, prier, klugy, monte, golem, velar, firer, pieta, umbel, campo, unpeg, fovea, abeam, boson, asker, goths, vocab, vined, trows, tikis, loper, indie, boffs, spang, grapy, tater, ichor, kilty, lochs, supes, degas, flics, torsi, beths, weber, resaw, lawny, coven, mujik, relet, therm, heigh, shnor, trued, zayin, liest, barfs, bassi, qophs, roily, flabs, punny, okras, hanks, dipso, nerfs, fauns, calla, pseud, lurer, magus, obeah, atria, twink, palmy, pocky, pends, recta, plonk, slaws, keens, nicad, pones, inker, whews, groks, mosts, trews, ulnar, gyppy, cocas, expos, eruct, oiler, vacua, dreck, dater, arums, tubal, voxel, dixit, beery, assai, lades, actin, ghoti, buzzy, meads, grody, ribby, clews, creme, email, pyxie, kulak, bocci, rived, duddy, hoper, lapin, wonks, petri, phial, fugal, holon, boomy, duomo, musos, shier, hayer, porgy, hived, litho, fisty, stagy, luvya, maria, smogs, asana, yogic, slomo, fawny, amine, wefts, gonad, twirp, brava, plyer, fermi, loges, niter, revet, unate, gyved, totty, zappy, honer, giros, dicer, calks, luxes, monad, cruft, quoin, fumer, amped, shlep, vinca, yahoo, vulva, zooey, dryad, nixie, moper, iambs, lunes, nudie, limns, weals, nohow, miaow, gouts, mynas, mazer, kikes, oxeye, stoup, jujus, debar, pubes, taels, defun, rands, blear, paver, goosy, sprog, oleos, toffy, pawer, maced, crits, kluge, tubed, sahib, ganef, scats, sputa, vaned, acned, taxol, plink, oweth, tribs, resay, boule, thous, haply, glans, maxis, bezel, antis, porks, quoit, alkyd, glary, beamy, hexad, bonks, tecum, kerbs, filar, frier, redux, abuzz, fader, shoer, couth, trues, guyed, goony, booky, fuzes, hurly, genet, hodad, calix, filer, pawls, iodic, utero, henge, unsay, liers, piing, weald, sexed, folic, poxed, cunts, anile, kiths, becks, tatty, plena, rebar, abled, toyer, attar, teaks, aioli, awing, anent, feces, redip, wists, prats, mesne, muter, smurf, owest, bahts, lossy, ftped, hunky, hoers, slier, sicks, fatly, delft, hiver, himbo, pengo, busks, loxes, zonks, ilium, aport, ikons, mulct, reeve, civvy, canna, barfy, kaiak, scudo, knout, gaper, bhang, pease, uteri, lases, paten, rasae, axels, stoas, ombre, styli, gunky, hazer, kenaf, ahoys, ammos, weeny, urger, kudzu, paren, bolos, fetor, nitty, techy, lieth, somas, darky, villi, gluon, janes, cants, farts, socle, jinns, ruing, slily, ricer, hadda, wowee, rices, nerts, cauls, swive, lilty, micks, arity, pasha, finif, oinky, gutty, tetra, wises, wolds, balds, picot, whats, shiki, bungs, snarf, legos, dungs, stogy, berms, tangs, vails, roods, morel, sware, elans, latus, gules, razer, doxie, buena, overs, gutta, zincs, nates, kirks, tikes, donee, jerry, mohel, ceder, doges, unmap, folia, rawly, snark, topoi, ceils, immix, yores, diest, bubba, pomps, forky, turdy, lawzy, poohs, worts, gloms, beano, muley, barky, tunny, auric, funks, gaffs, cordy, curdy, lisle, toric, soyas, reman, mungy, carpy, apish, oaten, gappy, aurae, bract, rooky, axled, burry, sizer, proem, turfy, impro, mashy, miens, nonny, olios, grook, sates, agley, corgi, dashy, doser, dildo, apsos, xored, laker, playa, selah, malty, dulse, frigs, demit, whoso, rials, sawer, spics, bedim, snugs, fanin, azoic, icers, suers, wizen, koine, topos, shirr, rifer, feral, laded, lased, turds, swede, easts, cozen, unhit, pally, aitch, sedum, coper, ruche, geeks, swags, etext, algin, offed, ninja, holer, doter, toter, besot, dicut, macer, peens, pewit, redox, poler, yecch, fluky, doeth, twats, cruds, bebug, bider, stele, hexer, wests, gluer, pilau, abaft, whelm, lacer, inode, tabus, gator, cuing, refly, luted, cukes, bairn, bight, arses, crump, loggy, blini, spoor, toyon, harks, wazoo, fenny, naves, keyer, tufas, morph, rajas, typal, spiff, oxlip, unban, mussy, finny, rimer, login, molas, cirri, huzza, agone, unsex, unwon, peats, toile, zombi, dewed, nooky, alkyl, ixnay, dovey, holey, cuber, amyls, podia, chino, apnea, prims, lycra, johns, primo, fatwa, egger, hempy, snook, hying, fuzed, barms, crink, moots, yerba, rhumb, unarc, direr, munge, eland, nares, wrier, noddy, atilt, jukes, ender, thens, unfix, doggo, zooks, diddy, shmoo, brusk, prest, curer, pasts, kelpy, bocce, kicky, taros, lings, dicky, nerdy, abend, stela, biggy, laved, baldy, pubis, gooks, wonky, stied, hypos, assed, spumy, osier, roble, rumba, biffy, pupal"
    database_freq = "3540,1877,2562,1766,2677,1228,1627,269,1580,105,1242,431,823,850,597,427,553,943,528,201,608,534,731,434,518,536,782,664,648,670,560,400,684,132,382,392,354,433,342,254,691,349,116,312,455,239,398,320,275,153,109,359,148,375,264,240,306,254,372,291,194,169,331,246,357,132,259,194,363,381,314,83,245,171,147,333,95,206,285,544,156,166,212,217,220,36,167,133,230,326,113,6,279,101,170,142,271,164,156,70,110,94,129,94,145,46,140,77,170,343,243,250,139,198,171,137,171,173,181,122,163,88,127,107,138,26,74,157,82,217,191,112,19,76,88,181,219,82,92,210,96,294,86,195,85,252,114,99,94,103,20,62,200,264,68,91,34,114,77,83,80,115,235,311,107,200,32,80,182,58,59,52,55,81,118,63,128,31,41,152,23,93,227,212,209,106,105,25,72,83,160,42,122,170,85,57,88,47,51,52,82,87,86,129,105,139,23,163,58,134,76,151,97,100,93,104,273,0,98,77,41,61,102,34,67,65,64,74,33,86,38,71,76,54,87,133,130,7,36,29,70,109,24,82,69,44,24,43,147,42,18,32,37,70,173,19,2,47,79,19,47,38,36,86,51,116,154,28,48,93,16,53,44,57,66,66,26,64,20,97,71,14,43,43,59,42,21,35,42,57,103,42,52,54,66,66,95,57,37,70,42,66,37,114,68,91,55,30,66,56,96,71,62,21,47,49,50,120,63,37,63,107,63,53,34,100,45,34,14,51,41,124,54,46,87,32,17,102,16,113,36,19,66,27,40,36,53,52,68,9,50,19,52,73,30,23,25,9,65,78,82,16,40,104,34,184,20,57,22,21,26,51,42,37,58,39,33,137,32,43,45,23,29,79,66,40,99,56,21,40,70,20,28,37,41,73,6,60,37,43,59,7,1,40,38,52,20,8,11,27,20,3,41,81,172,68,14,103,119,12,73,15,62,12,7,35,8,53,56,32,27,41,53,12,0,49,103,58,38,71,54,35,48,29,51,57,7,42,53,14,81,48,84,16,54,26,117,42,12,26,19,53,28,38,27,80,48,26,8,18,56,16,20,27,45,4,28,46,27,27,97,11,55,20,22,78,29,15,25,144,63,45,46,70,2,18,35,5,52,34,15,24,12,89,14,32,33,20,17,17,26,2,32,118,33,18,27,37,33,11,4,8,35,12,6,12,59,53,23,13,77,19,19,13,56,15,9,29,19,4,83,32,40,48,35,1,33,19,42,125,34,20,18,15,24,42,28,13,20,8,36,42,30,59,50,21,18,11,12,16,2,49,33,77,11,51,32,26,31,8,6,24,76,14,8,17,17,100,24,23,7,4,14,31,36,56,35,15,9,4,29,33,59,49,23,4,28,139,16,26,16,16,24,29,19,6,21,18,43,11,10,7,0,23,37,31,19,1,12,13,19,21,17,19,18,21,17,28,19,9,24,22,13,24,45,9,3,13,47,1,16,4,18,33,11,34,7,43,18,3,3,7,21,3,4,1,13,12,30,31,23,22,11,71,4,17,30,44,18,4,9,17,14,18,33,30,19,8,13,42,32,28,17,10,7,21,6,18,28,10,59,21,5,21,20,16,21,10,10,22,3,16,2,20,34,19,12,34,3,5,7,4,45,20,25,21,14,5,6,4,15,12,16,11,66,16,10,20,21,28,13,5,14,90,15,32,33,4,37,41,2,19,5,9,14,44,43,34,13,3,54,6,34,5,20,13,8,3,6,32,18,0,28,5,19,6,8,14,4,29,6,52,20,8,59,25,4,8,6,6,17,35,8,16,20,12,18,15,16,15,31,7,6,11,8,8,10,46,8,10,11,3,15,6,12,5,19,9,16,19,7,23,16,5,5,2,15,6,21,11,4,8,10,10,32,3,12,18,16,20,4,46,4,5,10,11,7,7,23,23,9,6,3,7,22,24,8,9,7,11,0,8,17,15,7,0,34,3,10,8,6,3,7,14,5,1,66,0,33,29,9,1,40,31,2,2,3,3,0,46,14,10,25,1,9,16,13,4,7,41,38,35,8,30,7,11,55,9,32,8,7,14,4,47,14,18,8,15,9,1,20,7,23,4,3,12,29,6,12,27,5,15,2,6,31,2,4,2,0,6,4,2,8,10,0,7,13,2,16,25,5,2,7,4,21,13,5,20,11,12,1,8,4,20,12,24,11,18,15,20,5,7,7,9,21,15,17,0,1,0,7,7,13,10,11,1,0,10,5,8,27,5,10,4,2,24,7,0,14,9,34,17,14,0,22,2,3,8,4,5,5,14,10,7,6,16,12,16,8,0,9,26,3,9,23,46,5,19,6,14,24,13,3,9,27,15,1,3,19,3,4,4,22,18,35,3,8,2,9,1,14,2,7,13,18,14,16,2,19,1,5,39,9,15,6,7,15,5,1,10,9,4,7,19,16,2,11,0,3,14,7,10,14,10,4,7,3,8,18,15,22,1,15,0,40,5,1,17,7,4,0,24,4,8,4,3,12,6,13,2,2,3,5,0,2,2,7,1,7,0,1,1,10,4,3,8,6,22,8,6,5,29,0,13,3,14,5,11,2,4,1,7,0,0,4,8,3,15,1,8,0,12,17,14,3,11,7,4,13,5,3,1,7,5,8,12,9,8,3,13,1,3,33,4,1,15,8,2,1,1,8,8,11,19,9,0,0,4,12,2,12,3,1,9,4,1,2,2,2,11,4,4,9,0,1,3,6,2,13,7,4,2,1,77,3,10,2,2,13,24,8,3,20,20,16,2,0,7,12,16,1,2,2,1,16,5,2,2,2,8,6,7,5,2,3,5,1,1,6,1,33,9,3,8,9,1,20,2,0,1,7,0,7,8,2,6,17,2,4,11,2,5,6,6,4,7,4,13,5,9,1,3,2,7,1,2,10,6,6,2,33,1,10,17,1,5,0,4,13,3,1,18,4,1,13,5,1,2,2,1,16,8,15,10,10,3,9,0,0,0,4,4,8,7,7,2,0,4,4,2,6,5,2,2,14,1,0,3,6,1,2,1,9,8,11,6,3,2,5,16,17,1,7,16,10,8,0,27,8,4,4,2,6,40,9,1,1,1,1,0,1,12,34,7,6,9,3,8,7,16,2,0,7,4,6,3,11,0,3,2,19,1,6,7,9,3,1,1,1,15,6,1,6,4,5,2,11,2,3,2,0,1,0,5,5,2,2,4,2,4,2,4,7,13,4,6,0,8,4,3,6,2,3,4,7,5,16,0,6,6,5,2,8,3,9,7,12,1,6,9,2,3,16,3,7,0,9,1,2,9,19,10,1,5,4,2,5,1,26,3,22,7,5,11,8,0,5,8,5,13,9,3,8,2,19,2,2,11,1,10,5,2,4,0,1,3,1,2,7,2,5,0,3,1,0,3,2,5,6,1,1,3,0,24,2,0,6,0,3,0,0,1,3,1,0,6,3,5,2,9,2,4,0,1,2,0,2,16,4,0,0,1,9,2,1,8,2,1,19,1,2,0,3,1,10,3,5,0,5,1,5,4,1,1,1,0,2,2,11,3,4,8,6,1,0,0,4,1,0,1,9,5,2,4,3,3,2,4,3,4,1,3,1,6,3,2,2,4,0,8,0,1,4,2,15,3,1,4,2,9,0,5,2,3,1,2,5,1,4,2,4,0,9,6,0,0,2,4,10,0,6,4,2,8,2,0,17,7,1,7,5,2,3,10,0,5,5,13,3,0,0,5,4,1,3,6,4,2,2,4,1,16,7,1,3,3,0,2,3,3,3,1,3,6,6,3,3,2,3,3,2,0,0,9,1,12,7,0,1,9,2,1,4,5,0,0,0,2,1,1,2,2,0,4,2,0,1,2,6,4,3,3,4,5,1,3,1,3,5,4,2,9,3,11,2,6,4,1,3,3,2,3,3,1,2,0,0,2,14,3,3,14,0,1,3,4,4,1,1,5,2,5,3,7,3,1,15,2,1,1,2,0,2,2,4,2,3,3,6,0,1,1,6,2,13,0,10,13,1,2,1,9,2,6,2,1,3,1,3,3,3,0,0,4,1,1,0,1,5,8,5,10,1,1,1,2,7,5,4,1,2,3,1,1,4,5,5,0,2,2,5,5,3,0,0,0,2,0,12,1,5,4,1,5,0,0,9,1,3,1,2,3,2,1,0,2,0,1,1,4,0,5,3,2,11,6,0,0,5,1,2,7,5,0,5,1,0,1,2,0,5,1,0,4,3,2,0,2,16,0,1,6,1,3,0,1,1,16,5,18,2,4,5,7,2,1,1,7,2,1,0,10,0,1,0,9,1,0,7,2,1,0,0,0,0,6,1,1,1,1,2,3,1,3,0,2,3,5,2,3,1,1,9,1,3,2,1,4,2,7,0,5,0,1,6,8,0,4,0,0,11,0,2,3,2,4,0,3,2,4,2,5,1,2,3,1,2,8,4,8,3,2,1,0,1,2,1,2,1,0,3,1,2,1,0,3,1,22,3,0,0,0,3,2,3,4,1,2,2,1,0,3,0,1,2,4,4,1,0,0,1,0,2,2,2,0,5,0,1,9,4,3,1,1,4,3,6,1,0,2,1,1,1,0,4,0,5,4,0,0,0,7,5,5,3,1,3,0,6,4,2,3,3,8,2,1,4,0,1,2,3,2,5,4,2,9,1,0,1,1,3,0,2,3,0,2,1,4,0,2,6,1,0,4,3,4,0,0,3,3,2,9,0,1,1,2,0,0,0,0,0,0,3,1,0,2,0,1,0,2,0,1,0,1,3,1,0,1,1,2,3,2,6,0,0,1,3,0,2,6,12,1,1,0,1,4,2,5,1,0,10,2,1,0,0,1,2,0,0,1,3,7,1,0,4,3,1,8,2,1,1,1,1,0,0,7,0,3,3,0,0,1,0,7,2,2,0,0,8,3,0,0,2,2,2,0,0,0,2,4,0,2,0,1,2,6,0,2,1,2,0,2,3,2,1,0,3,3,3,0,1,0,0,5,2,0,1,2,4,1,0,1,1,0,0,1,1,0,3,0,1,1,2,0,6,0,1,1,3,0,5,3,1,0,4,3,4,2,2,1,5,0,0,2,2,1,0,0,1,1,0,0,0,5,0,3,1,0,0,0,0,1,0,2,3,3,2,0,2,2,0,3,2,2,1,0,0,1,0,0,1,5,0,0,3,3,1,0,1,3,2,0,3,0,1,3,2,1,3,0,1,1,0,4,4,1,0,0,0,5,0,0,4,0,0,0,4,8,0,0,0,0,3,0,0,0,2,2,2,2,2,0,3,0,2,0,2,6,1,1,0,1,0,0,0,0,4,0,2,0,3,0,0,0,1,0,0,1,2,2,0,1,7,0,0,4,2,4,0,0,0,0,0,1,1,2,2,2,0,1,0,0,0,3,2,1,1,0,3,2,0,1,0,0,0,0,2,0,0,1,4,0,0,1,0,4,1,0,0,3,2,0,2,2,1,0,2,1,0,0,1,0,2,0,2,1,0,4,0,0,2,0,1,1,1,1,0,3,3,1,0,1,1,3,3,1,1,0,1,0,0,1,1,2,0,1,1,0,1,0,2,0,0,0,0,2,0,1,3,0,0,0,2,0,0,2,3,0,3,1,0,2,4,0,0,0,0,1,0,3,2,1,2,0,0,0,0,0,0,2,0,0,0,0,0,1,3,0,0,0,0,0,0,1,4,0,2,0,0,0,0,0,2,0,0,0,1,0,1,1,1,0,0,0,3,1,2,0,1,0,2,0,0,2,0,1,2,0,0,2,2,1,1,1,0,1,0,0,1,7,1,1,4,1,0,0,0,1,0,2,0,1,0,2,1,1,0,3,0,3,1,1,1,1,1,3,0,0,1,0,2,1,1,0,0,2,0,1,1,1,6,5,0,0,4,0,1,0,1,0,1,0,0,0,2,0,0,0,0,0,0,0,2,1,0,0,0,0,0,4,0,1,0,0,3,1,0,0,0,0,6,0,2,0,0,0,1,2,0,0,0,0,1,0,1,0,0,0,1,0,0,0,2,0,2,0,1,3,0,0,0,0,0,0,0,0,1,0,0,2,1,0,0,1,0,1,0,1,1,1,1,0,2,1,0,1,4,0,0,1,0,1,1,1,1,1,1,0,1,1,0,3,5,1,1,0,0,1,0,1,2,3,1,0,0,0,0,2,1,1,2,2,1,1,0,0,1,0,0,0,2,0,1,1,1,0,2,2,0,0,0,1,2,0,0,0,1,0,0,2,0,1,0,0,0,0,0,0,0,4,0,0,0,0,1,0,0,0,0,2,0,0,0,0,0,0,0,0,1,0,0,2,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,1,1,1,0,0,0,1,0,2,2,1,0,0,1,1,0,0,1,1,1,1,1,1,0,1,2,2,0,0,0,0,0,1,3,0,0,1,1,0,0,0,0,1,0,0,0,0,0,1,0,0,2,1,0,0,1,0,2,0,1,0,2,0,0,0,1,3,1,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,2,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,1,0,1,0,0,0,1,0,0,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,0,0,0,0,1,0,0,0,0,1,0,1,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,4,5,0,4,2,0,0,7,1,0,0,4,8,4,6,1,12,6,0,1,3,0,0,23,2,1,8,0,0,2,2,1,4,3,1,4,0,10,4,0,1,0,0,2,3,1,1,1,0,0,0,0,1,3,1,8,0,5,1,1,0,0,0,0,0,3,2,2,0,3,0,0,4,1,0,0,0,0,0,1,0,4,1,0,0,1,0,0,0,1,3,0,1,0,0,0,12,0,0,1,0,0,0,0,0,2,0,0,4,0,0,0,4,0,4,4,4,2,0,2,0,5,2,1,0,0,0,0,0,0,0,0,0,0,0,4,4,2,2,0,0,3,4,0,1,0,0,1,0,7,0,1,3,1,4,0,1,0,7,0,0,0,1,2,12,4,1,1,0,0,0,0,0,1,0,4,0,1,0,0,0,0,1,1,0,2,6,1,0,1,8,0,2,0,1,0,2,0,0,0,0,1,0,2,0,0,0,2,2,0,0,0,4,1,0,0,0,0,0,0,0,0,2,0,2,2,2,0,0,0,0,0,0,0,6,0,0,2,0,1,0,2,0,0,0,0,1,0,0,2,1,0,0,0,0,0,5,0,0,0,0,0,1,0,1,0,3,0,0,1,1,3,1,0,0,5,0,0,0,2,3,1,1,3,3,1,0,1,1,2,0,2,0,4,0,0,0,0,0,0,0,0,2,0,1,4,0,0,0,0,2,4,2,0,1,2,0,0,0,0,0,0,0,0,1,0,0,0,2,2,0,0,0,1,0,0,0,4,3,0,0,1,2,0,0,0,0,2,0,1,0,0,0,2,2,3,2,1,2,1,0,1,1,0,0,0,0,0,3,3,2,1,1,0,0,0,2,1,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,3,0,0,2,0,1,1,0,0,0,0,0,0,1,0,0,1,2,1,3,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,2,0,0,1,1,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,2,1,0,0,0,0,0,0,0,1,0,0,0,0,2,2,0,2,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,0,0,0,1,0,0,0,2,2,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,0,1,2,1,0,1,1,0,1,1,1,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,2,1,0,1,0,1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,1,0,0,2,1,0,1,1,0,0,1,1,0,6,1,0,0,0,0,0,1,0,0,0,1,1,0,1,1,0,1,0,1,2,1,0,0,0,0,1,0,1,1,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,4,2,0,0,0,1,0,0,0,0,0,0,2,0,0,0,0,0,0,1,1,0,1,0,0,3,1,1,0,0,0,1,0,0,3,1,2,2,0,2,0,0,1,0,1,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,2,2,0,0,0,2,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,1,0,0,0,1,0,1,0,0,1,0,0,0,1,1,0,0,0,1,0,1,1,1,1,2,0,1,0,1,1,1,1,0,0,1,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,1,1,0,0,1,1,0,1,0,1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0"
    
    databse_list = database.split(",")
    databse_list_freq = database_freq.split(",")    
    
    one_list = []
    two_list = []
    three_list = []
    four_list = []
    five_list = []

    databse_list_new = []
    one_list_new = []
    two_list_new = []
    three_list_new = []
    four_list_new = []
    five_list_new = []

    for w_dict in databse_list:
        w_dict = w_dict.strip()
        one_list.append(w_dict[0:1])
        two_list.append(w_dict[1:2])
        three_list.append(w_dict[2:3])
        four_list.append(w_dict[3:4])
        five_list.append(w_dict[4:5])
        
def is_present_there(x,list_):
    stopper_count = len(list_)
    global databse_list, databse_list_freq, one_list, two_list, three_list, four_list, five_list, databse_list_new, one_list_new,two_list_new,three_list_new,four_list_new,five_list_new
    index=0
    #print(x)
    for w in list_:   
        if(index>stopper_count):
            break
        if (w.upper()==x.upper()):
            #print(w)
            databse_list_new.append(databse_list[index])
            one_list_new.append(one_list[index])
            two_list_new.append(two_list[index])
            three_list_new.append(three_list[index])
            four_list_new.append(four_list[index])
            five_list_new.append(five_list[index])
            
        index=index+1
    databse_list = databse_list_new
    one_list = one_list_new
    two_list = two_list_new
    three_list = three_list_new
    four_list =  four_list_new 
    five_list =  five_list_new 
    databse_list_new = []
    one_list_new = []
    two_list_new = []
    three_list_new = []
    four_list_new = []
    five_list_new = []
    #print (databse_list)
    
def is_present_elsewhere(x,list_1,list_2,list_3,list_4,list_5):
    #print(x)    
    global databse_list, databse_list_freq, one_list, two_list, three_list, four_list, five_list, databse_list_new, one_list_new,two_list_new,three_list_new,four_list_new,five_list_new
    #print(len(databse_list))
    stopper_count = len(list_1)
    #print(len(list_1))
    
    index=0
    for w in list_1:
        if(index>stopper_count):
            break
        #print(index,len(list_1))
        if (w.upper()!=x.upper()):
            inclusion_list = [list_2[index].upper(),list_3[index].upper(),list_4[index].upper(),list_5[index].upper()]
            if ((x.upper()==list_2[index].upper())or(x.upper()==list_3[index].upper())or(x.upper()==list_4[index].upper())or(x.upper()==list_5[index].upper())):
                #print(inclusion_list)
                databse_list_new.append(databse_list[index])
                one_list_new.append(one_list[index])
                two_list_new.append(two_list[index])
                three_list_new.append(three_list[index])
                four_list_new.append(four_list[index])
                five_list_new.append(five_list[index])
        index=index+1

    databse_list = databse_list_new
    one_list = one_list_new
    two_list = two_list_new
    three_list = three_list_new
    four_list =  four_list_new 
    five_list =  five_list_new  
    databse_list_new = []
    one_list_new = []
    two_list_new = []
    three_list_new = []
    four_list_new = []
    five_list_new = []
    #print(databse_list)
    
def del_the_word(word_):
    #print("Calling del" + word_)
    global databse_list, databse_list_freq, one_list, two_list, three_list, four_list, five_list, databse_list_new, one_list_new,two_list_new,three_list_new,four_list_new,five_list_new
    loc_databse_list = databse_list.copy()
    index=0
    for w in loc_databse_list:
        #print(w,word_)
        if(w.upper().strip()==word_.upper().strip()):      
            #print("deleted "+ w.upper())
            databse_list[index] = "DEL_"
            one_list[index] = "DEL_"
            two_list[index] = "DEL_"
            three_list[index] = "DEL_"
            four_list[index] = "DEL_"
            five_list[index] = "DEL_"
            databse_list_freq[index] = "-1"
            
        index=index+1
        
    databse_list = [x for x in databse_list if x != "DEL_"]
    one_list = [x for x in one_list if x != "DEL_"]
    two_list = [x for x in two_list if x != "DEL_"]
    three_list = [x for x in three_list if x != "DEL_"]
    four_list = [x for x in four_list if x != "DEL_"]
    five_list = [x for x in five_list if x != "DEL_"]
    databse_list_freq = [x for x in databse_list_freq if x != "-1"]
  
def del_char(x):
    try:
        global databse_list, databse_list_freq, one_list, two_list, three_list, four_list, five_list, databse_list_new, one_list_new,two_list_new,three_list_new,four_list_new,five_list_new
        index=0
        loc_databse_list = databse_list.copy()
        #print("Calling del for char" + x)
        #print("Calling del for char" + x + " in " + str(databse_list))
        for w in loc_databse_list:
            #print(w,x)
            index_char=w.upper().strip().find(x.upper())
            if(index_char!=-1):      
                #print("deleted "+ w.upper())
                databse_list[index] = "DEL_"
                one_list[index] = "DEL_"
                two_list[index] = "DEL_"
                three_list[index] = "DEL_"
                four_list[index] = "DEL_"
                five_list[index] = "DEL_"
                databse_list_freq[index] = "-1"
            index=index+1
        
        databse_list = [x for x in databse_list if x != "DEL_"]
        one_list = [x for x in one_list if x != "DEL_"]
        two_list = [x for x in two_list if x != "DEL_"]
        three_list = [x for x in three_list if x != "DEL_"]
        four_list = [x for x in four_list if x != "DEL_"]
        five_list = [x for x in five_list if x != "DEL_"]
        databse_list_freq = [x for x in databse_list_freq if x != "-1"]
        
    except:
        ("Did you type anything wrong")        

def find_index(rem, list_find):
    index = 0
    out = 0
    m_val = 0
    for val in databse_list_freq:
        if(int(val) > int(m_val)):
            m_val = val
            out = index
        index = index+1      
    
    return out
    
def play_(s1,s2,s3):
    try:
        remain_words =""
        word=""
        for count in range(10):
            #print(actual_word,len(databse_list))
            found =0
            if(len(databse_list)==0):
                break

            index = -1

            if(len(databse_list)<10):
                index_e=1
                list_elements =""
                s2 = ""
                s3 = ""

                for element in databse_list:
                    list_elements = list_elements + "(" + str(index_e) + ")" + element
                    index_e = index_e+1



            if(mode.upper()=="A")or(len(databse_list)>10):
                index = find_index(remain_words,databse_list)
            elif(mode.upper()=="I"):
                print(list_elements)
                index_in = input('Select index for the word you want to select from the list')
                index = int(index_in)
            #print(index, len(databse_list))
            word = (databse_list[index-1]).strip().upper()            

            if (count==0):
                word=s1

            if (count==1) &(s2!=""):
                word=s2
                
            if (count==2)&(s3!=""):
                word=s3
                
            #if (count==2):
            #    word="CRYPT"
            

            print ("Type this word in Wordle ->  " + word)
            del_the_word(word)

            #c,a = index_correct(word,actual_word)
            #print(word)
            #print(a)
            c = input ('index of right alphabets at right position (green ones) seperated by comma->')
            remain_words = "1,2,3,4,5"
 
            if(c==""):
                c="0"
            c=c.replace(" ","")

            if(c=="E")or(c=="e"):
                c="1,2,3,4,5"

            if(c=="1,2,3,4,5"):
                print("Hurray solve in " + str(count+1) + " steps")
                return str(count+1)

            if(c!="0"):        
                for in_c in c:
                    in_c = in_c.strip()
                    if(in_c==','):
                        continue
                    if (int(in_c)==1):
                        is_present_there(word[int(in_c)-1],one_list)
                    if (int(in_c)==2):
                        is_present_there(word[int(in_c)-1],two_list)
                    if (int(in_c)==3):
                        is_present_there(word[int(in_c)-1],three_list)
                    if (int(in_c)==4):
                        is_present_there(word[int(in_c)-1],four_list)
                    if (int(in_c)==5):
                        is_present_there(word[int(in_c)-1],five_list)

            #c,a = index_correct(word,actual_word)
            a = input ('index of right alphabets at wrong position (brown ones) seperated by comma->')
            if(a==""):
                a="0"
            a=a.replace(" ","")

            if(a=="E")or(a=="E"):
                print("Exiting from the program")
                raise Exception()
            if(a!="0"):
                list_a = []
                for in_a in a:   
                    in_a=in_a.strip()
                    if(in_a==','):
                        continue                

                    if (int(in_a)==1):
                        is_present_elsewhere(word[int(in_a)-1],one_list,two_list,three_list,four_list,five_list)                   
                    if (int(in_a)==2):
                        is_present_elsewhere(word[int(in_a)-1],two_list,one_list,three_list,four_list,five_list)
                    if (int(in_a)==3):
                        is_present_elsewhere(word[int(in_a)-1],three_list,one_list,two_list,four_list,five_list)
                    if (int(in_a)==4):
                        is_present_elsewhere(word[int(in_a)-1],four_list,one_list,two_list,three_list,five_list)
                    if (int(in_a)==5):
                        is_present_elsewhere(word[int(in_a)-1],five_list,one_list,two_list,three_list,four_list)

            tot =""

            if(str(a)!="0"):
                tot = tot + str(a)

            if(str(a)!="0") and (str(c)!="0"):
                tot = tot + ","

            if(str(c)!="0"):
                tot = tot + str(c)

            
            list_tot = tot.split(",")
            found = found + (len(list_tot))
            if(found>3):
                s2 = ""
                s3 = ""

            del_list = ["1","2","3","4","5"]
            for ind_ex in list_tot:
                #print(ind_ex)
                if(ind_ex==","):
                    continue
                try:
                    if (str(ind_ex)=="1"):
                        #print("removing " + ind_ex)
                        del_list.remove("1")
                    if (str(ind_ex)=="2"):
                        #print("removing " + ind_ex)
                        del_list.remove("2")
                    if (str(ind_ex)=="3"):
                        #print("removing " + ind_ex)
                        del_list.remove("3")
                    if (str(ind_ex)=="4"):
                        #print("removing " + ind_ex)
                        del_list.remove("4")
                    if (str(ind_ex)=="5"):
                        #print("removing " + ind_ex)
                        del_list.remove("5")
                except:
                    abcdefg=1
            #print(del_list,word)

            for del_word in del_list:
                word_del = word[int(del_word)-1]
                #print(word_del)
                #print(list_tot)
                for pre_word in list_tot:
                    #print("*"+pre_word)
                    if(pre_word!=""):
                        oth_word = word[int(pre_word)-1]
                        #print(oth_word)
                        if (word_del==oth_word):
                            word_del =""

                if(word_del!=""):
                    #print(word_del)
                    del_char(word_del)

            #print(databse_list)
    except e:
        print("Closing" +e)
        return "exit"
        
try:
    play="Y"
    while(play.upper()=="Y"):
        initialize_()
        ex = play_("AROSE","BLUNT","CHIMP")
        #print(ex)
        play = input("Do you want to play again (Y/N)")
except e:
    print("Final Exit. Thanks" + e)  