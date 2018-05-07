from rest_framework.views import APIView
from rest_framework.response import Response
from .query import GetData


class ComputeScore(APIView):
    """
    View to compute scorecard for given request id
    """

    def get(self, request):
        """
        Return a list of all users.
        """
        response = {
            'success': False
        }
        request_id = request.get('request_id')
        query = GetData(request_id)
        data_frame = query.execute()
        return Response(response)
