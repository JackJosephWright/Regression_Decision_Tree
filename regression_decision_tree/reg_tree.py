import dash
from dash import html
import dash_pro_components as pro

app = dash.Dash(__name__)

elements=[
    # Nodes elements
    {
        'id': 'input_node',
        'type': 'input',
        'data': {'label': 'Input Node'},
        'position': {'x': 300, 'y': 25},
        'sourcePosition': 'bottom'
    },
    {
        'id': 'custom_node',
        'type': 'customDefault',
        'data': {
            'label': 'Custom Node',
            'style': {'background-color': '#b1b1b7'}
        }, 
        'position': {'x': 200, 'y': 125}

    },
    {
        'id': 'default_node_1',
        'data': {'label': 'Default Node 1'}, 
        'position': {'x': 400, 'y': 125},
        'targetPosition': 'top',
        'sourcePosition': 'bottom'

    },
    {
        'id': 'default_node_2',
        'data': {'label': 'Default Node 2'}, 
        'position': {'x': 200, 'y': 225}

    },
    {
        'id': 'output_node_1',
        'type': 'output',
        'data': {'label': 'Output Node 1'},
        'position': {'x': 100, 'y': 325},
        'targetPosition': 'top'
    },
    {
        'id': 'output_node_2',
        'type': 'output',
        'data': {'label': 'Output Node 2'},
        'position': {'x': 350, 'y': 300},
        'targetPosition': 'top'
    },

    # Edge elements
    {
        'id': 'input_node-custom_node',
        'source': 'input_node',
        'target': 'custom_node',
        'animated': True,
        'label': 'Animated Edge'
    },
    {
        'id': 'input_node-default_node_1',
        'source': 'input_node',
        'target': 'default_node_1',
        'label': 'Default Edge'
    },
    {
        'id': 'custom_node-default_node_2',
        'source': 'custom_node',
        'target': 'default_node_2',
        'arrowHeadType': 'arrowclosed',
        'label': 'With arrow head',
        'style': {'stroke': 'black'},
        'labelStyle': {'fill': 'black', 'fontWeight': 700}
    },
    {
        'id': 'default_node_2-output_node_1',
        'source': 'default_node_2',
        'target': 'output_node_1',
        'type': 'smoothstep',
        'label': 'Smooth step edge',
        'style': {'stroke': '#ff0072'},
        'labelStyle': {'fill': '#ff0072', 'fontWeight': 700}
    },
    {
        'id': 'default_node_2-output_node_2',
        'source': 'default_node_2',
        'target': 'output_node_2',
        'type': 'step',
        'label': 'Step edge',
        'animated': True,
        'style': {'stroke': '#ff0072'},
        'labelStyle': {'fill': '#ff0072', 'fontWeight': 700}
    }
]

app.layout = html.Div([
    pro.FlowChart(
        id='flowchart-layout',
        style={'width': '100%', 'height': '600px'},
        elements=elements,
        arrowHeadColor='black'
    )
])

if __name__ == '__main__':
    app.run_server()
