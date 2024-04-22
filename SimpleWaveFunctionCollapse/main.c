#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>

#define MAP_WIDTH  20
#define MAP_HEIGHT 20

#define MASK(tile) (1<<(tile))
#define TILE_FOUND(option, tile) (((option) >> (tile)) & 0x1)
#define PRINT_IF_EQUAL(format, val, enum) (((val) == (enum)) ? printf(format, #enum) : 0)

// Enumerations
typedef enum {
    EMPTY = 0,
    AIR,
    DIRT,
    DIRT_L,
    DIRT_R,
    GRASS,
    GRASS_L,
    GRASS_R,
    CLOUD,
    CLOUD_T,
    CLOUD_B,
    CLOUD_L,
    CLOUD_R,
    CLOUD_TL,
    CLOUD_TR,
    CLOUD_BL,
    CLOUD_BR,
    TILE_COUNT,
} tile_t;

typedef enum {
    UP,
    RIGHT,
    DOWN,
    LEFT,
    DIRECTION_COUNT,
} direction_t;

// Function Prototypes
void print_map(tile_t map[MAP_HEIGHT][MAP_WIDTH], const char tile_chars[TILE_COUNT]);
void print_options(uint32_t options[MAP_HEIGHT][MAP_WIDTH], const char tile_chars[TILE_COUNT]);
void print_rules(uint32_t rules[TILE_COUNT][DIRECTION_COUNT]);
int update_map(tile_t map[MAP_HEIGHT][MAP_WIDTH], uint32_t options[MAP_HEIGHT][MAP_WIDTH]);
void update_options(uint32_t options[MAP_HEIGHT][MAP_WIDTH], tile_t map[MAP_HEIGHT][MAP_WIDTH], uint32_t rules[TILE_COUNT][DIRECTION_COUNT]);
void update_rules(uint32_t rules[TILE_COUNT][DIRECTION_COUNT], const uint8_t sockets[TILE_COUNT][DIRECTION_COUNT]);
uint8_t count_legal_options(uint32_t option_mask);
tile_t get_legal_option(uint32_t option_mask, uint8_t option_num);

// Function Definitions
int main(int argc, char* argv[]) {
    srand(time(NULL));

    assert(TILE_COUNT == 17);
    const uint8_t SOCKETS[TILE_COUNT][DIRECTION_COUNT] = {
        [AIR] = {
            [UP]    = 1,
            [RIGHT] = 1,
            [DOWN]  = 1,
            [LEFT]  = 1,
        },
        [DIRT] = {
            [UP]    = 2,
            [RIGHT] = 3,
            [DOWN]  = 2,
            [LEFT]  = 3,
        },
        [DIRT_L] = {
            [UP]    = 4,
            [RIGHT] = 3,
            [DOWN]  = 4,
            [LEFT]  = 1,
        },
        [DIRT_R] = {
            [UP]    = 5,
            [RIGHT] = 1,
            [DOWN]  = 5,
            [LEFT]  = 3,
        },
        [GRASS] = {
            [UP]    = 1,
            [RIGHT] = 6,
            [DOWN]  = 2,
            [LEFT]  = 6,
        },
        [GRASS_L] = {
            [UP]    = 1,
            [RIGHT] = 6,
            [DOWN]  = 4,
            [LEFT]  = 1,
        },
        [GRASS_R] = {
            [UP]    = 1,
            [RIGHT] = 1,
            [DOWN]  = 5,
            [LEFT]  = 6,
        },
        [CLOUD] = {
            [UP]    = 7,
            [RIGHT] = 7,
            [DOWN]  = 7,
            [LEFT]  = 7,
        },
        [CLOUD_T] = {
            [UP]    = 1,
            [RIGHT] = 8,
            [DOWN]  = 7,
            [LEFT]  = 8,
        },
        [CLOUD_B] = {
            [UP]    = 7,
            [RIGHT] = 9,
            [DOWN]  = 1,
            [LEFT]  = 9,
        },
        [CLOUD_L] = {
            [UP]    = 10,
            [RIGHT] = 7,
            [DOWN]  = 10,
            [LEFT]  = 1,
        },
        [CLOUD_R] = {
            [UP]    = 11,
            [RIGHT] = 1,
            [DOWN]  = 11,
            [LEFT]  = 7,
        },
        [CLOUD_TL] = {
            [UP]    = 1,
            [RIGHT] = 8,
            [DOWN]  = 10,
            [LEFT]  = 1,
        },
        [CLOUD_TR] = {
            [UP]    = 1,
            [RIGHT] = 1,
            [DOWN]  = 11,
            [LEFT]  = 8,
        },
        [CLOUD_BL] = {
            [UP]    = 10,
            [RIGHT] = 9,
            [DOWN]  = 1,
            [LEFT]  = 1,
        },
        [CLOUD_BR] = {
            [UP]    = 11,
            [RIGHT] = 1,
            [DOWN]  = 1,
            [LEFT]  = 9,
        },
    };

    assert(TILE_COUNT == 17);
    const char TILE_CHARS[TILE_COUNT] = {
        [EMPTY] = ' ',
        [AIR] = '.',
        [DIRT] = '#',
        [DIRT_L] = '#',
        [DIRT_R] = '#',
        [GRASS] = '_',
        [GRASS_L] = '_',
        [GRASS_R] = '_',
        [CLOUD] = '*',
        [CLOUD_T] = '*',
        [CLOUD_B] = '*',
        [CLOUD_L] = '*',
        [CLOUD_R] = '*',
        [CLOUD_TL] = '*',
        [CLOUD_TR] = '*',
        [CLOUD_BL] = '*',
        [CLOUD_BR] = '*',
    };

    tile_t map[MAP_HEIGHT][MAP_WIDTH] = {};
    tile_t map_ref[MAP_HEIGHT][MAP_WIDTH] = {};
    uint32_t options[MAP_HEIGHT][MAP_WIDTH] = {};
    uint32_t rules[TILE_COUNT][DIRECTION_COUNT] = {};

    // print_map(map, TILE_CHARS);
    // print_options(options, TILE_CHARS);

    update_rules(rules, SOCKETS);
    // print_rules(rules);

    // Initial map settings
    for (size_t i = 0; i < MAP_WIDTH; i++) {
        map_ref[0][i] = AIR;
        // map_ref[MAP_HEIGHT - 1][i] = DIRT;
    }

    memcpy(map, map_ref, sizeof(map));
    // TODO: Check for completion
    for (uint32_t i = 0; i < MAP_WIDTH * MAP_HEIGHT + 1; i++) {
        update_options(options, map, rules);
        // print_options(options, TILE_CHARS);
        if (update_map(map, options)) {
            printf("FAILED on iteration: %d!!!!\n", i);

            // Restart until it works
            memcpy(map, map_ref, sizeof(map));
            i = 0;
        }
        // print_map(map, TILE_CHARS);
    }
    print_map(map, TILE_CHARS);
}

void print_map(tile_t map[MAP_HEIGHT][MAP_WIDTH], const char tile_chars[TILE_COUNT]) {

    for (size_t x = 0; x < MAP_WIDTH; x++) {
        printf("=");
    }
    printf("\n");

    for (size_t y = 0; y < MAP_HEIGHT; y++) {
        for (size_t x = 0; x < MAP_WIDTH; x++) {
            printf("%c", tile_chars[map[y][x]]);
        }
        printf("\n");
    }

    for (size_t x = 0; x < MAP_WIDTH; x++) {
        printf("=");
    }
    printf("\n");
}

void print_options(uint32_t options[MAP_HEIGHT][MAP_WIDTH], const char tile_chars[TILE_COUNT]) {

    for (size_t x = 0; x < MAP_WIDTH; x++) {
        printf("%.*s", TILE_COUNT + 1,"=================================");
    }
    printf("=\n");

    for (size_t y = 0; y < MAP_HEIGHT; y++) {
        for (size_t x = 0; x < MAP_WIDTH; x++) {
            printf(" ");
            for (size_t t = 0; t < TILE_COUNT; t++) {
                TILE_FOUND(options[y][x], t) ? printf("%c", tile_chars[t]) : printf(" ");
            }
        }
        printf("\n");
    }

    for (size_t x = 0; x < MAP_WIDTH; x++) {
        printf("%.*s", TILE_COUNT + 1,"=================================");
    }
    printf("=\n");
}

void print_rules(uint32_t rules[TILE_COUNT][DIRECTION_COUNT]) {
    assert(TILE_COUNT == 17);
    assert(DIRECTION_COUNT == 4);

    for (tile_t t = EMPTY; t < TILE_COUNT; t++) {
        PRINT_IF_EQUAL("[%s] = {\n", t, EMPTY);
        PRINT_IF_EQUAL("[%s] = {\n", t, AIR);
        PRINT_IF_EQUAL("[%s] = {\n", t, DIRT);
        PRINT_IF_EQUAL("[%s] = {\n", t, DIRT_L);
        PRINT_IF_EQUAL("[%s] = {\n", t, DIRT_R);
        PRINT_IF_EQUAL("[%s] = {\n", t, GRASS);
        PRINT_IF_EQUAL("[%s] = {\n", t, GRASS_L);
        PRINT_IF_EQUAL("[%s] = {\n", t, GRASS_R);
        PRINT_IF_EQUAL("[%s] = {\n", t, CLOUD);
        PRINT_IF_EQUAL("[%s] = {\n", t, CLOUD_T);
        PRINT_IF_EQUAL("[%s] = {\n", t, CLOUD_B);
        PRINT_IF_EQUAL("[%s] = {\n", t, CLOUD_L);
        PRINT_IF_EQUAL("[%s] = {\n", t, CLOUD_R);
        PRINT_IF_EQUAL("[%s] = {\n", t, CLOUD_TL);
        PRINT_IF_EQUAL("[%s] = {\n", t, CLOUD_TR);
        PRINT_IF_EQUAL("[%s] = {\n", t, CLOUD_BL);
        PRINT_IF_EQUAL("[%s] = {\n", t, CLOUD_BR);
        for (direction_t d = 0; d < DIRECTION_COUNT; d++) {
            PRINT_IF_EQUAL("  [%5s] = { ", d, UP);
            PRINT_IF_EQUAL("  [%5s] = { ", d, RIGHT);
            PRINT_IF_EQUAL("  [%5s] = { ", d, DOWN);
            PRINT_IF_EQUAL("  [%5s] = { ", d, LEFT);
            for (tile_t allowed_tile = 0; allowed_tile < TILE_COUNT; allowed_tile++) {
                if (TILE_FOUND(rules[t][d], allowed_tile)) {
                    PRINT_IF_EQUAL("%s ", allowed_tile, EMPTY);
                    PRINT_IF_EQUAL("%s ", allowed_tile, AIR);
                    PRINT_IF_EQUAL("%s ", allowed_tile, DIRT);
                    PRINT_IF_EQUAL("%s ", allowed_tile, DIRT_L);
                    PRINT_IF_EQUAL("%s ", allowed_tile, DIRT_R);
                    PRINT_IF_EQUAL("%s ", allowed_tile, GRASS);
                    PRINT_IF_EQUAL("%s ", allowed_tile, GRASS_L);
                    PRINT_IF_EQUAL("%s ", allowed_tile, GRASS_R);
                    PRINT_IF_EQUAL("%s ", allowed_tile, CLOUD);
                    PRINT_IF_EQUAL("%s ", allowed_tile, CLOUD_T);
                    PRINT_IF_EQUAL("%s ", allowed_tile, CLOUD_B);
                    PRINT_IF_EQUAL("%s ", allowed_tile, CLOUD_L);
                    PRINT_IF_EQUAL("%s ", allowed_tile, CLOUD_R);
                    PRINT_IF_EQUAL("%s ", allowed_tile, CLOUD_TL);
                    PRINT_IF_EQUAL("%s ", allowed_tile, CLOUD_TR);
                    PRINT_IF_EQUAL("%s ", allowed_tile, CLOUD_BL);
                    PRINT_IF_EQUAL("%s ", allowed_tile, CLOUD_BR);
                }
            }
            printf("}\n");
        }
        printf("}\n");
    }
}

int update_map(tile_t map[MAP_HEIGHT][MAP_WIDTH], uint32_t options[MAP_HEIGHT][MAP_WIDTH]) {
    static size_t min_locations[MAP_HEIGHT * MAP_WIDTH];
    uint8_t smallest_count = -1;
    size_t num_minimums = 0;
    uint8_t _tmp_count = 0;
    size_t _choice = 0;

    // Find argmin(s)
    for (size_t y = 0; y < MAP_HEIGHT; y++) {
        for (size_t x = 0; x < MAP_WIDTH; x++) {
            if (map[y][x]) continue;

            _tmp_count = count_legal_options(options[y][x]);
            if (_tmp_count < smallest_count) {
                smallest_count = _tmp_count;
                num_minimums = 0;
            }

            if (_tmp_count == smallest_count) {
                min_locations[num_minimums++] = y * MAP_WIDTH + x;
            }
        }
    }

    // printf("MIN_COUNT: %d, ", smallest_count);
    // printf("ARGMIN(s): [ ");
    // for (size_t i = 0; i < num_minimums; i++) {
    //     printf("%ld ", min_locations[i]);
    // }
    // printf("]\n");

    // Return if there are no options
    if ((num_minimums == 0) && (smallest_count == ((uint8_t) -1)))
    {
        return 0; // No more changes to make
        // TODO: Somehow indicate that the map is full
    }
    else if ((num_minimums == 0) || (smallest_count == 0))
    {
        return 1;  // Indicate error
    }

    // Change random argmin
    _choice = min_locations[rand() % num_minimums];
    // printf("Choosing: %d\n", _choice);

    map[0][_choice] = get_legal_option(options[0][_choice], (rand() % smallest_count) + 1);

    return 0;
}

void update_options(uint32_t options[MAP_HEIGHT][MAP_WIDTH], tile_t map[MAP_HEIGHT][MAP_WIDTH], uint32_t rules[TILE_COUNT][DIRECTION_COUNT]) {
    for (size_t y = 0; y < MAP_HEIGHT; y++) {
        for (size_t x = 0; x < MAP_WIDTH; x++) {
            options[y][x] = (uint32_t)(-1);
            if (map[y][x]) {
                options[y][x] = MASK(map[y][x]);
                continue;
            }

            if (x > 0)              options[y][x] &= rules[map[y][x - 1]][RIGHT];
            if (x < MAP_WIDTH - 1)  options[y][x] &= rules[map[y][x + 1]][LEFT];
            if (y > 0)              options[y][x] &= rules[map[y - 1][x]][DOWN];
            if (y < MAP_HEIGHT - 1) options[y][x] &= rules[map[y + 1][x]][UP];
        }
    }
}

void update_rules(uint32_t rules[TILE_COUNT][DIRECTION_COUNT], const uint8_t sockets[TILE_COUNT][DIRECTION_COUNT]) {
    rules[EMPTY][UP]    = -1;
    rules[EMPTY][RIGHT] = -1;
    rules[EMPTY][DOWN]  = -1;
    rules[EMPTY][LEFT]  = -1;

    assert(DIRECTION_COUNT == 4);
    for (tile_t t = EMPTY + 1; t < TILE_COUNT; t++) {
        for (direction_t d = 0; d < DIRECTION_COUNT; d++) {
            rules[t][d] = 0;
            for (tile_t neighbor = EMPTY + 1; neighbor < TILE_COUNT; neighbor++) {
                if (sockets[t][d] == sockets[neighbor][(d + 2) % 4]) {
                    rules[t][d] |= MASK(neighbor);
                }
            }
        }
    }
}

uint8_t count_legal_options(uint32_t option_mask) {
    uint8_t count = 0;
    for (tile_t t = EMPTY; t < TILE_COUNT; t++) {
        count += TILE_FOUND(option_mask, t);
    }

    return count;
}

tile_t get_legal_option(uint32_t option_mask, uint8_t option_num) {
    uint8_t n = 0;
    for (tile_t tile = EMPTY; tile < TILE_COUNT; tile++) {
        n += TILE_FOUND(option_mask, tile);
        if (n == option_num) {
            return tile;
        }
    }

    return EMPTY;
}
