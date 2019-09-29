import numpy as np

states_count = 6
actions_count = 6
step_count = 6


def generate_world():
    whole = np.zeros((states_count, actions_count, states_count, 2))
    return whole


def initialize_agent():
    current = np.random.randint(states_count)
    values = np.random.random(states_count)
    policies = np.random.choice(actions_count, states_count)
    return {'current': current, 'values': values, 'policies': policies}


def make_a_step():
    state = agent['current']
    action = agent['policies'][state]
    state_prime = np.random.choice(states_count, 1, p=world[state, action, :, 0])
    instant_reward = world[state, action, state_prime, 1]

    agent['current'] = state_prime
    agent['values'] = None
    agent['policies'] = None


world = generate_world()
agent = initialize_agent()

for k in range(step_count):
    make_a_step()
