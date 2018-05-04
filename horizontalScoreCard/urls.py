from django.conf.urls import url
from . import views

urlpatterns = [
    url(r'^horizontal-score-card/$', views.post_list, name='post_list'),
]